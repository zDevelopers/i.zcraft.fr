<?php

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Generator\UrlGeneratorInterface;

$app = require '../bootstrap.php';


// Routes & controllers ----------------------------------------

/*
 * Front page (upload form)
 */
$app->get('/', function (Request $request) use ($app) {
    return $app['render']($request, 'index.html.twig', ['config' => $app['config']]);
})
->bind('home');


/*
 * Upload processing + links page
 */
$app->post('/', function (Request $request) use ($app) {
    $file = $request->files->get('image');

    // Check if the file was uploaded
    if ($file == null || !$file->isValid())
    {
        $app->abort(400);
    }

    $mime_type = $file->getMimeType(); // Cannot be accessed after $file->move()

    if (!in_array($mime_type, $app['config']['allowed_mime_types'])
        || $file->getClientSize() > $file->getMaxFilesize()
        || $file->getClientSize() > $app['config']['max_file_size'])
    {
        $app->abort(400);
    }


    $system_function_allowed = function_exists('shell_exec');


    // Upload itself

    $storage_name = mt_rand(100000, 999999) . time() . '.' . $file->guessExtension();
    $storage_path = substr($storage_name, 0, 2) . '/' . substr($storage_name, 2, 2) . '/';
    $full_storage_path = $app['config']['storage_dir'] . '/' . $storage_path . $storage_name;

    $stored_file = $file->move($app['config']['storage_dir'] . '/' . $storage_path, $storage_name);
    $base_uri = $request->getSchemeAndHttpHost() . $request->getBasePath() . '/'
                                                 . $app['config']['public_storage_dir']
                                                 . (!$app['config']['strip_folders'] ? $storage_path : '');
    $file_uri = $base_uri . $storage_name;


    // EXIF deletion

    if ($request->request->get('remove_exif', false) && in_array($mime_type, ['image/jpeg', 'image/tiff']))
    {
        if ($app['config']['use_system_exif_tools'] && $system_function_allowed)
        {
            shell_exec('convert -auto-orient ' . $full_storage_path . ' ' . $full_storage_path);
            shell_exec('exiftool -overwrite_original -all= ' . $full_storage_path);
        }

        // Our fallback only supports JPEG images :c
        elseif ($mime_type == 'image/jpeg')
        {
            // First we fix the orientation, if needed.
            fix_image_orientation($full_storage_path);

            // Then EXIF data can be removed...
            rename($full_storage_path, $full_storage_path . '.exif');
            @remove_exif($full_storage_path . '.exif', $full_storage_path);
            unlink($full_storage_path . '.exif');
        }
    }


    // Thumbnail generation

    $mini_name = 'mini_' . $storage_name;
    $mini_path = $app['config']['storage_dir'] . '/' . $storage_path . $mini_name;
    $mini_uri  = $base_uri . $mini_name;
    $thmb_size = $app['config']['thumb_size'];

    $resized = false;

    if ($app['config']['use_system_convert'] && $system_function_allowed)
    {
        shell_exec('convert ' . $full_storage_path . ' -resize \'' . $thmb_size . 'x' . $thmb_size . '>\' ' . $mini_path);
        $resized = true;
    }
    elseif (in_array($mime_type, ['image/png', 'image/jpeg', 'image/gif']))
    {
        $resized = make_thumbnail($full_storage_path, $mini_path, $thmb_size);
    }

    // We cannot create a thumbnail :(
    if (!$resized)
    {
        copy($full_storage_path, $mini_path);
    }


    // Data aggregation

    $deletion_token = random_string(32);
    $expiration_date = -1;

    if ($request->request->has('expires'))
    {
        $expires_after = intval($request->request->get('expires_after'));
        $expiration_date = $expires_after == 0 ? 0 : time() + $expires_after;
    }

    $image_data = [
        'original_name'     => $file->getClientOriginalName(),
        'storage_name'      => $storage_name,
        'storage_path'      => $storage_path . $storage_name,
        'storage_path_mini' => $storage_path . $mini_name,
        'url'               => $file_uri,
        'url_mini'          => $mini_uri,
        'uploaded_at'       => time(),
        'uploaded_by'       => $request->getClientIp(),
        'deletion_token'    => $deletion_token,
        'expires_at'        => $expiration_date,
        'deleted'           => false
    ];


    // Saves deletion token and expiration

    $db = get_db();
    $q = $db->prepare('INSERT INTO images (storage_name, storage_path, storage_path_mini, url, url_mini, original_name, '
                      . 'uploaded_at, uploaded_by, deletion_token, expires_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)');
    $q->execute([
        $image_data['storage_name'], $image_data['storage_path'], $image_data['storage_path_mini'], $image_data['url'],
        $image_data['url_mini'],  $image_data['original_name'], $image_data['uploaded_at'], $image_data['uploaded_by'],
        $image_data['deletion_token'], $image_data['expires_at']
    ]);

    // User view

    return $app['render']($request, 'links.html.twig',
    [
        'full_url' => $file_uri,
        'mini_url' => $mini_uri,
        'delete_url' => $app['url_generator']->generate('delete', ['token' => $deletion_token], UrlGeneratorInterface::ABSOLUTE_URL),
        'deletion_token' => $deletion_token,
        'image' => $image_data,
        'config' => $app['config']
    ]);
})
->bind('upload');


/*
 * Deletion links processing
 */
$app->get('/delete/{token}', function (Request $request, $token) use ($app) {
    $db = get_db();
    $q = $db->prepare('SELECT * FROM images WHERE deletion_token = :token AND deleted = 0');
    $q->execute([':token' => $token]);
    $image = $q->fetch(PDO::FETCH_ASSOC);

    if ($image)
        delete_image($image, $app['config']['storage_dir']);

    return $app['render']($request, 'deleted.html.twig',
    [
        'deleted' => (bool) $image,
        'image' => $image
    ], $image != null ? 200 : 404);
})
->bind('delete');


/*
 * API endpoint to check if an image is deleted without triggering
 * first-view deletion. This only displays the status, and cannot
 * be used to access the image without triggering the deletion
 * (for images deleted at first view). Returns a 404 if there
 * was no such image.
 *
 * Endpoint: /api/exists?i=image_name.png
 * Returns JSON: {'storage_name': '<image storage name>', 'deleted': true/false}
 */
$app->get('/api/exists', function(Request $request) use ($app) {
    if (!$request->query->has('i'))
        $app->abort(400);

    $image = get_image(extract_image_name($request->query->get('i')));
    if ($image === false)
        $app->abort(404);

    return $app->json([
        'storage_name' => $image['storage_name'],
        'deleted'      => (bool) $image['deleted']
    ]);
});


/**
 * Errors pages controller
 */
$app->error(function (\Exception $e, Request $request, $code) use ($app) {
    $template = null;

    switch ($code)
    {
        case 404:
        case 400:
            $template = 'error_' . $code;
            break;

        default:
            $template = 'error';
    }

    return $app['render']($request, $template . '.html.twig', ['code' => $code]);
});


$app['debug'] = in_array($_SERVER['SERVER_NAME'], array('0.0.0.0', '127.0.0.1', 'localhost'));
$app->run();
