<?php
namespace IZcraft\Command;

use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Input\InputArgument;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Style\SymfonyStyle;
use Symfony\Component\Routing\Generator\UrlGeneratorInterface;

class ImageHitCommand extends \Knp\Command\Command
{
    protected function configure()
    {
        $this->setName("i:hit")
             ->setDescription("Handles hit on an image.")
             ->setHelp('This command should be called by the webserver after an image has been sent to a client. It is used to delete images at first view, if needed.')
             ->addArgument('image', InputArgument::REQUIRED, 'The name or URL of the image', null);
    }

    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $db = get_db();
        $image_name = extract_image_name($input->getArgument('image'));
        $image = get_image($image_name);

        $io = new SymfonyStyle($input, $output);

        if ($image === false)
        {
            $io->error('Image ' . $image_name . ' not found.');
            return;
        }

        // Image deleted at first view?
        if ($image['expires_at'] == 0 && !$image['deleted'])
        {
            delete_image($image, $this->getSilexApplication()['config']['storage_dir']);
            $io->success('Image ' . $image['storage_name'] . ' was set to be deleted at first view; purged.');
        }
    }
}
