<?php
namespace IZcraft\Command;

use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Input\InputArgument;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Style\SymfonyStyle;
use Symfony\Component\Routing\Generator\UrlGeneratorInterface;

class PurgeImagesCommand extends \Knp\Command\Command
{
    protected function configure()
    {
        $this->setName("i:purge")
             ->setDescription("Removes the expired images from the filesystem.")
             ->setHelp('This command will delete all expired images from the file system, if not already deleted.');
    }

    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $db = load_db();
        $storage_dir = $this->getSilexApplication()['config']['storage_dir'];
        $deleted = 0;

        $io = new SymfonyStyle($input, $output);

        foreach ($db['images'] as $index => $image)
        {
            $updated_image = delete_image_if_expired($image, $storage_dir);
            if ($updated_image !== false)
            {
                $io->text('Deleted expired image ' . $updated_image['storage_name']);
                $deleted++;

                $db['images'][$index] = $updated_image;
            }
        }

        if ($deleted > 0) save_db($db);

        $io->success('Successfully purged ' . $deleted . ' expired image' . ($deleted != 1 ? 's' : '') . '.');
    }
}
