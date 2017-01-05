<?php
namespace IZcraft\Command;

use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Input\InputArgument;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Style\SymfonyStyle;
use Symfony\Component\Routing\Generator\UrlGeneratorInterface;


class ShowImageCommand extends \Knp\Command\Command
{
    protected function configure()
    {
        $this->setName("i:show")
             ->setDescription("Displays details on an image.")
             ->setHelp('This command allows you to display details on an uploaded file')
             ->addArgument('image', InputArgument::REQUIRED, 'The name or URL of the image', null);
    }

    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $db = load_db();
        $image_name = extract_image_name($input->getArgument('image'));
        $image = get_image($db, $image_name);

        $io = new SymfonyStyle($input, $output);

        if ($image === false)
        {
            $io->error('Image ' . $image_name . ' not found.');
            return;
        }

        $io->text('<comment>Details for image ' . $image_name . '</>');
        $io->newLine();

        $expires = '';
        switch ($image['expires_at']) {
            case -1:
                $expires = 'never'; break;

            case 0:
                $expires = 'at first view'; break;

            default:
                $expires  = $image['expires_at'] < time() ? 'already expired ' : '';
                $expires .= date('d/m/Y \a\t H:i:s', $image['expires_at']) . ', ' . ($image['expires_at'] < time() ? '' : 'in ');
                $expires .= format_date_diff(new \DateTime(), new \DateTime('@'.$image['expires_at'])) . ($image['expires_at'] < time() ? ' ago' : '');
        }

        $io->table([], [
            ['<info>Name</>', $image['storage_name']],
            ['<info>Original name</>', $image['original_name']],
            ['<info>Storage path</>', $this->getSilexApplication()['config']['storage_dir'] . '/' . $image['storage_path']],
            ['<info>Thumbnail path</>', $this->getSilexApplication()['config']['storage_dir'] . '/' . $image['storage_path_mini']],
            ['<info>URL</>', $image['url']],
            ['<info>Thumbnail URL</>', $image['url_mini']],
            ['<info>Uploaded at</>', date('d/m/Y H:i:s', $image['uploaded_at'])],
            ['<info>Uploaded by</>', $image['uploaded_by']],
            ['<info>Expires</>', $expires],
            ['<info>Deleted</>', isset($image['deleted']) && $image['deleted'] ? 'yes' : 'no'],
            ['<info>Deletion token</>', $image['deletion_token']]
        ]);
    }
}
