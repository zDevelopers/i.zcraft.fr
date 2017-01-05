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
        $this->setName("images:list")
             ->setDescription("Lists the images in the database")
             ->setHelp('This command allows you to list the images uploaded by users. The list can be filtered using command arguments.')
             ->addOption('show-deleted', null, InputOption::VALUE_NONE, 'Pass to display deleted images.')
             ->addOption('in-name', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters the original files names. Multiple values will be ORed.', [])
             ->addOption('from-ip', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters by uploader IP address.  Multiple values will be ORed.', []);
    }

    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $db = load_db();
        $displayed = 0;

        $show_deleted = $input->getOption('show-deleted') != null;
        $filter_names = $input->getOption('in-name');
        $filter_ips   = $input->getOption('from-ip');

        $io = new SymfonyStyle($input, $output);
        $table = [];

        foreach ($db['images'] as $image)
        {
            if (!$show_deleted && isset($image['deleted']) && $image['deleted']) continue;
            if (!$this->filter($filter_names, $image['original_name'])) continue;
            if (!$this->filter($filter_ips, $image['uploaded_by'])) continue;

            $table[] = [
                $image['storage_name'] . ' (' . $image['original_name'] . ')',
                date('d/m/Y H:i:s', $image['uploaded_at']) . ' by ' . $image['uploaded_by'],
                $image['expires_at'] > -1 ? date('d/m/Y H:i:s', $image['expires_at']) : 'never',
                isset($image['deleted']) && $image['deleted'] ? 'yes' : 'no'
            ];

            $displayed++;
        }

        if (empty($table))
        {
            $io->error('No image matched your query. (' . count($db['images']) . ' images total.)');
        }
        else
        {
            $io->table(['Name (client name)', 'Uploaded', 'Expires', 'Deleted'], $table);
            $io->text('Total: ' . $displayed . ' out of ' . count($db['images']) . ' images total.');
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
