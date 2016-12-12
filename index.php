<?php
require_once __DIR__ . '/vendor/autoload.php';

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Generator\UrlGeneratorInterface;


$app = new Silex\Application();


// App config (or create config.php to override) --------------

$app['config'] =
[
    'storage_dir' => 'static',
    'public_storage_dir' => 'static',
    'strip_folders' => false,

    'allowed_mime_types' => ['image/png', 'image/jpeg', 'image/gif', 'image/bmp', 'image/tiff'],
    'max_file_size' => 1024 * 1024 * 32,

    'thumb_size' => 300,

    'use_system_convert' => true,

    'data_file' => 'db.php'
];



// App bootstrap ----------------------------------------------

require_once 'utils.php';

$app->register(new Silex\Provider\TwigServiceProvider(), [
    'twig.path' => __DIR__.'/templates',
]);

if (file_exists('config.php')) $app['config'] = array_merge($app['config'], include('config.php'));



// Routes & controllers ----------------------------------------

$app->get('/', function () use ($app)
{
    return $app['twig']->render('index.html.twig');
})
->bind('home');


$app->post('/', function (Request $request) use ($app)
{
    $file = $request->files->get('image');
    $mime_type = $file->getMimeType(); // Cannot be accessed after $file->move()

    if (!$file->isValid()
        || !in_array($mime_type, $app['config']['allowed_mime_types'])
        || $file->getClientSize() > $file->getMaxFilesize()
        || $file->getClientSize() > $app['config']['max_file_size'])
    {
        $app->abort(400);
    }


    // Upload itself

    $storage_name = mt_rand(100000,999999) . time() . '.' . $file->guessExtension();
    $storage_path = substr($storage_name, 0, 2) . '/' . substr($storage_name, 2, 2) . '/';
    $full_storage_path = $app['config']['storage_dir'] . '/' . $storage_path . $storage_name;

    $stored_file = $file->move($app['config']['storage_dir'] . '/' . $storage_path, $storage_name);

    $base_uri = $request->getURI() . $app['config']['public_storage_dir'] . '/' . (!$app['config']['strip_folders'] ? $storage_path : '');
    $file_uri = $base_uri . $storage_name;

    
    // Thumbnail generation

    $mini_name = 'mini_' . $storage_name;
    $mini_path = $app['config']['storage_dir'] . '/' . $storage_path . $mini_name;
    $mini_uri  = $base_uri . $mini_name;
    $thmb_size = $app['config']['thumb_size'];

    $resized = false;

    if ($app['config']['use_system_convert'] && function_exists('system'))
    {
        system('convert ' . $full_storage_path . ' -resize \'' . $thmb_size . 'x' . $thmb_size . '>\' ' . $mini_path);
        $resized = true;
    }
    else if (in_array($mime_type, ['image/png', 'image/jpeg', 'image/gif']))
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
    $expiration_date = $request->request->has('expires') ? time() + intval($request->request->get('expires_after')) : -1;

    $image_data = [
        'original_name'     => $file->getClientOriginalName(),
        'storage_name'      => $storage_name,
        'storage_path'      => $full_storage_path,
        'storage_path_mini' => $mini_path,
        'url'               => $file_uri,
        'url_mini'          => $mini_uri,
        'uploaded_at'       => time(),
        'uploaded_by'       => $request->getClientIp(),
        'deletion_token'    => $deletion_token,
        'expires_at'        => $expiration_date
    ];


    // Saves deletion token and expiration

    $db = load_db();
    if (!is_array($db['images'])) $db['images'] = [];

    $db['images'][] = $image_data;

    save_db($db);


    // User view

    return $app['twig']->render('links.html.twig',
    [
        'full_url' => $file_uri,
        'mini_url' => $mini_uri,
        'delete_url' => $app['url_generator']->generate('delete', ['token' => $deletion_token], UrlGeneratorInterface::ABSOLUTE_URL),
        'deletion_token' => $deletion_token,
        'image' => $image_data
    ]);
})
->bind('upload');


$app->get('/delete/{token}', function ($token) use ($app)
{
    $db = load_db();
    $image = null;

    if (is_array($db['images']))
    {
        $len = count($db['images']);
        for ($i = 0; $i < $len; $i++)
        {
            if ($db['images'][$i]['deletion_token'] != $token || $db['images'][$i]['deleted']) continue;

            $image = $db['images'][$i];

            unlink(__DIR__ . '/' . $image['storage_path']);
            unlink(__DIR__ . '/' . $image['storage_path_mini']);

            $image['deleted'] = true;
            $db['images'][$i] = $image;

            break;
        }
    }

    save_db($db);

    return new Response($app['twig']->render('deleted.html.twig',
    [
        'deleted' => $image != null,
        'image' => $image
    ]), $image != null ? 200 : 404);
})
->bind('delete');


/**
 * Debug tool
 */
$app->get('/db', function () use ($app)
{
    if (!$app['debug']) $app->abort(404);

    echo '<pre>';
    var_dump(load_db());
    echo '</pre>';

    $dump = ob_get_clean();
    ob_end_clean();
    return $dump;
});


$app->error(function (\Exception $e, Request $request, $code) use ($app)
{
    $template = null;

    switch ($code)
    {
        case 404:
            $template = 'error_' . $code;
            break;

        default:
            $template = 'error';
    }

    return $app['twig']->render($template . '.html.twig', ['code' => $code]);
});

$app['debug'] = in_array($_SERVER['SERVER_NAME'], array('0.0.0.0', '127.0.0.1', 'localhost'));
$app->run();
