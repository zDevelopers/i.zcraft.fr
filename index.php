<?php
require_once __DIR__ . '/vendor/autoload.php';

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;


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

$app->register(new Silex\Provider\TwigServiceProvider(), [
    'twig.path' => __DIR__.'/templates',
]);

/**
 * Loads the saved data (here, an array).
 * In the file, data is serialized,compressed and encoded in base64.
 */
function load_db()
{
    global $app; // TODO improve (service?)
    $data_file = $app['config']['data_file'];

    if (!is_file($data_file))
    {
        return [];
    }
    else
    {
        return unserialize(gzinflate(base64_decode(substr(file_get_contents($data_file), 8))));
    }
}

/**
 * Saves the data in a file.
 * Returns the success of the operation.
 */
function save_db($data)
{
    global $app;
    return file_put_contents($app['config']['data_file'], '<?php //' . base64_encode(gzdeflate(serialize($data))));
}


if (file_exists('config.php'))
{
    $app['config'] = array_merge($app['config'], include('config.php'));
}



// Routes & contollers ----------------------------------------

$app->get('/', function() use($app)
{
    return $app['twig']->render('index.html.twig');
});


$app->post('/', function(Request $request) use($app)
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

    // Fallback to GD to generate thump without convert
    else if (in_array($mime_type, ['image/png', 'image/jpeg', 'image/gif']))
    {
        list($original_width, $original_height, $original_type) = getimagesize($full_storage_path);

        if ($original_width > $original_height)
        {
            $new_width  = $thmb_size;
            $new_height = intval($original_height * $new_width / $original_width);
        }
        else
        {
            $new_height = $thmb_size;
            $new_width  = intval($original_width * $new_height / $original_height);
        }

        $dest_x = intval(($thmb_size - $new_width) / 2);
        $dest_y = intval(($thmb_size - $new_height) / 2);

        if ($original_type === 1)
        {
            $image_save_function   = "ImageGIF";
            $image_create_function = "ImageCreateFromGIF";
        }
        else if ($original_type === 2)
        {
            $image_save_function   = "ImageJPEG";
            $image_create_function = "ImageCreateFromJPEG";
        }
        else if ($original_type === 3)
        {
            $image_save_function   = "ImagePNG";
            $image_create_function = "ImageCreateFromPNG";
        }

        if ($image_save_function)
        {
            $old_image = $image_create_function($full_storage_path);
            $new_image = imagecreatetruecolor($thmb_size, $thmb_size);

            // apply transparent background only if is a png image
            if($original_type === 3)
            {
                imagesavealpha($new_image, TRUE);
                $color = imagecolorallocatealpha($new_image, 0, 0, 0, 127);
                imagefill($new_image, 0, 0, $color);
            }

            imagecopyresampled($new_image, $old_image, $dest_x, $dest_y, 0, 0, $new_width, $new_height, $original_width, $original_height);
            $image_save_function($new_image, $mini_path);

            $resized = true;
        }
    }

    // We cannot create a thumbnail :(
    if (!$resized)
    {
        copy($full_storage_path, $mini_path);
    }

    return $app['twig']->render('links.html.twig',
    [
        'full_url' => $file_uri,
        'mini_url' => $mini_uri,
        'delete_url' => 'https://i.zcraft.fr/delete/mhVNBPpqpMiWZEdAzLKMY6MGxLstYUk4'
    ]);
});


$app['debug'] = true;
$app->run();
