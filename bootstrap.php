<?php

require_once __DIR__ . '/vendor/autoload.php';

use Knp\Provider\ConsoleServiceProvider;


$app = new Silex\Application();



// App config (or create config.php to override) --------------

$app['config'] =
[
    'storage_dir' => __DIR__ . '/web/static',
    'public_storage_dir' => 'static',
    'strip_folders' => false,

    'allowed_mime_types' => ['image/png', 'image/jpeg', 'image/gif', 'image/bmp', 'image/tiff'],
    'max_file_size' => 1024 * 1024 * 32,

    'thumb_size' => 300,

    'use_system_convert' => true,

    'data_file' => __DIR__ . '/db.php'
];



// App bootstrap ----------------------------------------------

require_once 'utils.php';

$app->register(new Silex\Provider\TwigServiceProvider(), [
    'twig.path' => __DIR__.'/templates',
]);

$app->register(new ConsoleServiceProvider(), [
    'console.name' => 'izcraft',
    'console.version' => '2.0',
    'console.project_directory' => __DIR__ . "/.."
]);

if (file_exists('config.php')) $app['config'] = array_merge($app['config'], include('config.php'));



// ------------------------------------------------------------

return $app;
