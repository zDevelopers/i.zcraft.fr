<?php
namespace IZcraft\Command;

use PDO;
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
        $db = get_db();
        $storage_dir = $this->getSilexApplication()['config']['storage_dir'];
        $deleted = 0;

        $io = new SymfonyStyle($input, $output);
        $q = $db->prepare('SELECT storage_name, storage_path, storage_path_mini FROM images
                           WHERE deleted = 0 AND expires_at > 0 AND expires_at < strftime(\'%s\', \'now\')');
        $q->execute();

        while ($image = $q->fetch(PDO::FETCH_ASSOC))
        {
            delete_image($image, $storage_dir);

            $io->text('Deleted expired image ' . $image['storage_name']);
            $deleted++;
        }

        $io->success('Successfully purged ' . $deleted . ' expired image' . ($deleted != 1 ? 's' : '') . '.');
    }
}
