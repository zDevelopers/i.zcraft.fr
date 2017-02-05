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
             ->addOption('in-name', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters the original files names. Multiple values will be ORed.', [])
             ->addOption('from-ip', null, InputOption::VALUE_REQUIRED | InputOption::VALUE_IS_ARRAY, 'Filters by uploader IP address.  Multiple values will be ORed.', []);
    }

    protected function execute(InputInterface $input, OutputInterface $output)
    {
        $db = get_db();

        $show_deleted = $input->getOption('show-deleted') != null;
        $show_tokens  = $input->getOption('with-tokens') != null;
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

        while ($image = $q->fetch(PDO::FETCH_ASSOC))
        {
            $row = [
                $image['storage_name'], $image['original_name'],
                date('d/m/Y H:i:s', $image['uploaded_at']) . ' by ' . $image['uploaded_by'],
                $image['expires_at'] > -1 ? $image['expires_at'] == 0 ? 'at first view' : date('d/m/Y H:i:s', $image['expires_at']) : 'never',
                $image['deleted'] ? 'yes' : 'no'
            ];

            if ($show_tokens) $row[] = $image['deletion_token'];

            $table[] = $row;
        }

        if (empty($table))
        {
            $io->error('No image matched your query. (' . $images_count . ' images total.)');
        }
        else
        {
            $header = ['Name', 'Client name', 'Uploaded', 'Expires', 'Deleted'];
            if ($show_tokens) $header[] = 'Deletion token';

            $io->table($header, $table);
            $io->text(count($table) . ' out of ' . $images_count . ' images total.');
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
