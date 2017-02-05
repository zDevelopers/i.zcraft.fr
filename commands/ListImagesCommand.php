<?php
namespace IZcraft\Command;

use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Input\InputOption;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Style\SymfonyStyle;

class ListImagesCommand extends \Knp\Command\Command
{
    protected function configure()
    {
        $this->setName("i:list")
             ->setDescription("Lists the images in the database.")
             ->setHelp('This command allows you to list the images uploaded by users. The list can be filtered using command arguments.')
             ->addOption('show-deleted', null, InputOption::VALUE_NONE, 'Pass to display deleted images.')
             ->addOption('with-tokens', null, InputOption::VALUE_NONE, 'Pass to display deletion tokens.')
             ->addOption('in-name', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters the original files names. Multiple values will be ORed.', [])
             ->addOption('from-ip', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters by uploader IP address.  Multiple values will be ORed.', []);
    }

    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $db = load_db();

        $show_deleted = $input->getOption('show-deleted') != null;
        $show_tokens  = $input->getOption('with-tokens') != null;
        $filter_names = $input->getOption('in-name');
        $filter_ips   = $input->getOption('from-ip');

        $io = new SymfonyStyle($input, $output);
        $table = [];

        foreach ($db['images'] as $image)
        {
            if (!$show_deleted && isset($image['deleted']) && $image['deleted']) continue;
            if (!$this->filter($filter_names, $image['original_name'])) continue;
            if (!$this->filter($filter_ips, $image['uploaded_by'])) continue;

            $row = [
                $image['storage_name'], $image['original_name'],
                date('d/m/Y H:i:s', $image['uploaded_at']) . ' by ' . $image['uploaded_by'],
                $image['expires_at'] > -1 ? $image['expires_at'] == 0 ? 'at first view' : date('d/m/Y H:i:s', $image['expires_at']) : 'never',
                isset($image['deleted']) && $image['deleted'] ? 'yes' : 'no'
            ];

            if ($show_tokens) $row[] = $image['deletion_token'];

            $table[] = $row;
        }

        if (empty($table))
        {
            $io->error('No image matched your query. (' . count($db['images']) . ' images total.)');
        }
        else
        {
            $header = ['Name', 'Client name', 'Uploaded', 'Expires', 'Deleted'];
            if ($show_tokens) $header[] = 'Deletion token';

            $io->table($header, $table);
            $io->text(count($table) . ' out of ' . count($db['images']) . ' images total.');
        }
    }

    private function filter($filter_list, $field)
    {
        $pass = true;

        if (!empty($filter_list))
        {
            $pass = false;
            foreach ($filter_list as $item)
            {
                if (strpos(strtolower($field), strtolower($item)) !== false)
                {
                    $pass = true;
                    break;
                }
            }
        }

        return $pass;
    }
}
