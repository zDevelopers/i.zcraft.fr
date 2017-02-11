<?php
namespace IZcraft\Command;

use PDO;
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
             ->addOption('with-sizes', null, InputOption::VALUE_NONE, 'Pass to display files sizes.')
             ->addOption('in-name', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters the original files names. Multiple values will be ORed.', [])
             ->addOption('from-ip', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters by uploader IP address.  Multiple values will be ORed.', []);
    }

    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $db = get_db();

        $show_deleted = $input->getOption('show-deleted') != null;
        $show_tokens  = $input->getOption('with-tokens') != null;
        $show_sizes   = $input->getOption('with-sizes') != null;
        $filter_names = $input->getOption('in-name');
        $filter_ips   = $input->getOption('from-ip');

        $io = new SymfonyStyle($input, $output);
        $table = [];

        $q = $db->prepare('SELECT COUNT(*) AS count FROM images');
        $q->execute();
        $images_count = $q->fetch(PDO::FETCH_ASSOC)['count'];

        $where_clauses = [];
        $parameters = [];

        if (!$show_deleted) $where_clauses[] = 'deleted = 0';
        $this->filter($filter_names, 'original_name', $where_clauses, $parameters);
        $this->filter($filter_ips, 'uploaded_by', $where_clauses, $parameters);

        $q = $db->prepare('SELECT * FROM images' . (!empty($where_clauses) ? ' WHERE ' . implode(' AND ', $where_clauses) : ''));
        $q->execute($parameters);

        $total_size = 0;

        while ($image = $q->fetch(PDO::FETCH_ASSOC))
        {
            $row = [];

            $row[] = $image['storage_name'];
            $row[] = $image['original_name'];

            if ($show_sizes)
            {
                $size_original = $size_thumbnail = false;

                if (!$image['deleted'])
                {
                    $size_original  = filesize($this->getSilexApplication()['config']['storage_dir'] . '/' . $image['storage_path']);
                    $size_thumbnail = filesize($this->getSilexApplication()['config']['storage_dir'] . '/' . $image['storage_path_mini']);
                }

                if ($size_original !== false)
                {
                    $size = $size_original + ($size_thumbnail !== false ? $size_thumbnail : 0);
                    $total_size += $size;

                    $row[] = human_filesize($size);
                }
                else
                {
                    $row[] = '-';
                }
            }

            $row[] = date('d/m/Y H:i:s', $image['uploaded_at']) . ' by ' . $image['uploaded_by'];
            $row[] = $image['expires_at'] > -1 ? $image['expires_at'] == 0 ? 'at first view' : date('d/m/Y H:i:s', $image['expires_at']) : 'never';
            $row[] = $image['deleted'] ? 'yes' : 'no';

            if ($show_tokens) $row[] = $image['deletion_token'];

            $table[] = $row;
        }

        if (empty($table))
        {
            $io->error('No image matched your query. (' . $images_count . ' images total.)');
        }
        else
        {
            $header = ['Name', 'Client name'];
            if ($show_sizes)  $header[] = 'Size';
            $header = array_merge($header, ['Uploaded', 'Expires', 'Deleted']);
            if ($show_tokens) $header[] = 'Deletion token';

            $io->table($header, $table);
            $io->text(count($table) . ' out of ' . $images_count . ' images total.');

            if ($show_sizes) $io->text('Total size: ' . human_filesize($total_size) . '.');
        }
    }

    private function filter($filter_list, $field, array &$where_clauses, array &$parameters)
    {
        if (!empty($filter_list))
        {
            $test = [];
            foreach ($filter_list as $item)
            {
                $test[] = $field . ' LIKE ?';
                $parameters[] = '%' . $item . '%';
            }
            $where_clauses[] = '(' . implode(' OR ', $test) . ')';
        }
    }
}
