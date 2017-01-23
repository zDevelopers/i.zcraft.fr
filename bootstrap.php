<?php

require_once __DIR__ . '/vendor/autoload.php';

use Knp\Provider\ConsoleServiceProvider;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use \Twig_Error_Loader;


$app = new Silex\Application();



// App config (or create config.php to override) --------------

$app['config'] =
[
    'storage_dir' => __DIR__ . '/web/static',
    'public_storage_dir' => 'static',
    'strip_folders' => false,

    'allowed_mime_types' => ['image/png', 'image/jpeg', 'image/gif', 'image/bmp', 'image/tiff'],
    'max_file_size' => 1024 * 1024 * 32,

    'features' => [
        // If this is enabled, a cron must call the command
        // ./console --quiet i:purge every few minutes.
        'expiration' => true,


        // If this is enabled, the webserver must be configured to call
        // ./console i:hit --quiet <url> when an image was sent to a client.
        // Requires expiration enabled.
        'deletion_at_first_view' => true
    ],

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


/**
 * Renders a view, but replaces the template with a CLI template if the client is WGet or CURL.
 * In this case a template “name.html.twig” will be replated by “name.cli.twig”, if the template exists.
 *
 * @return Response
 */
$app['render'] = $app->protect(function(Request $request, $template_name, array $context = array(), $http_code = 200) use($app)
{
    $user_agent = strtolower(trim($request->headers->get('User-Agent', '')));
    $plain_text = false;

    $template = null;

    if (strpos($user_agent, 'wget') === 0 || strpos($user_agent, 'curl') === 0)
    {
        try
        {
            $template = $app['twig']->load('cli/' . str_replace('.html.twig', '.cli.twig', $template_name));
            $plain_text = true;
        }
        catch (Twig_Error_Loader $e) {}
    }

    if ($template == null)
    {
        $template = $app['twig']->load($template_name);
    }

    return new Response($template->render($context), $http_code, ['Content-Type' => $plain_text ? 'text/plain' : 'text/html']);
});



// ------------------------------------------------------------

return $app;
