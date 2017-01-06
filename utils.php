<?php

/**
 * Loads the saved data (here, an array).
 * In the file, data is serialized,compressed and encoded in base64.
 *
 * @return array Stored data
 */
function load_db()
{
    global $app; // TODO improve (service?)
    $data_file = $app['config']['data_file'];

    if (!is_file($data_file))
    {
        $db = [];
    }
    else
    {
        $db = unserialize(gzinflate(base64_decode(substr(file_get_contents($data_file), 8))));
    }

    if (!isset($db['images']) || !is_array($db['images'])) $db['images'] = [];

    return $db;
}

/**
 * Saves the data in a file.
 * Returns the success of the operation.
 *
 * @param array $data The data to save
 *
 * @return int|boolean False on failure.
 */
function save_db($data)
{
    global $app;
    return file_put_contents($app['config']['data_file'], '<?php //' . base64_encode(gzdeflate(serialize($data))));
}

/**
 * Retrieves an image in the database from it's storage name.
 *
 * @return array|boolean The image data or false if not found.
 */
function get_image($db, $storage_name)
{
    foreach ($db['images'] as $image)
    {
        if ($storage_name == $image['storage_name'])
        {
            return $image;
        }
    }

    return false;
}

/**
 * Saves an image in the database.
 *
 * @param array $db The database.
 * @param array $image The image to save into the database.
 * @return array The new database.
 */
function set_image($db, $image)
{
    foreach ($db['images'] as $index => $db_image)
    {
        if ($db_image['storage_name'] == $image['storage_name'])
        {
            $db['images'][$index] = $image;
        }
    }

    return $db;
}

/**
 * Deletes an image.
 *
 * @param array $image An array containing the image data.
 * @param string $root The storage root directory.
 * @return The new image data to store into the database.
 */
function delete_image($image, $root)
{
    unlink($root . '/' . $image['storage_path']);
    unlink($root . '/' . $image['storage_path_mini']);

    $image['deleted'] = true;
    return $image;
}

/**
 * Deletes an image if expired.
 *
 * @param array $image An array containing the image data.
 * @param string $root The storage root directory.
 * @return The new image data, if the image was deleted, or false if nothing was changed.
 */
function delete_image_if_expired($image, $root)
{
    if (!$image['deleted'] && $image['expires_at'] > -1 && $image['expires_at'] != 0 && $image['expires_at'] <= time())
    {
        return delete_image($image, $root);
    }

    else return false;
}


/**
 * Extracts the name of an image.
 *
 * @param string $image The image name or URL
 * @return The image name.
 */
function extract_image_name($image)
{
    if (strpos($image, 'mini_') !== false)
    {
        $image = str_replace('mini_', '', $image);
    }

    if (strpos($image, '/') !== false)
    {
        return array_pop(explode('/', $image));
    }

    else return $image;
}


/**
 * Generates and writes a thumbnail.
 *
 * @var $original_path string The path to the original file to reduce.
 * @var $mini_path     string The path to the reduced file to write.
 * @var $thumb_size    int The thumbnail size wanted.
 *
 * @return bool Success
 */
function make_thumbnail($original_path, $mini_path, $thumb_size)
{
    list($original_width, $original_height, $original_type) = getimagesize($original_path);

    if ($original_width < $original_height)
    {
        $new_height = $thumb_size;
        $new_width  = intval($original_width * $new_height / $original_height);
    }
    else
    {
        $new_width  = $thumb_size;
        $new_height = intval($original_height * $new_width / $original_width);
    }

    $dest_x = intval(($thumb_size - $new_width) / 2);
    $dest_y = intval(($thumb_size - $new_height) / 2);

    $image_save_function = false;
    $image_create_function = false;

    if ($original_type === 1)
    {
        $image_save_function   = "ImageGIF";
        $image_create_function = "ImageCreateFromGIF";
    }
    else if ($original_type === 2)
    {
        $image_save_function   = "ImageJPEG";
        $image_create_function = "ImageCreateFromJPEG";
    }
    else if ($original_type === 3)
    {
        $image_save_function   = "ImagePNG";
        $image_create_function = "ImageCreateFromPNG";
    }

    if ($image_save_function)
    {
        $old_image = $image_create_function($original_path);
        $new_image = imagecreatetruecolor($thumb_size, $thumb_size);

        // apply transparent background only if is a png image
        if($original_type === 3)
        {
            imagesavealpha($new_image, TRUE);
            $color = imagecolorallocatealpha($new_image, 0, 0, 0, 127);
            imagefill($new_image, 0, 0, $color);
        }

        imagecopyresampled($new_image, $old_image, $dest_x, $dest_y, 0, 0, $new_width, $new_height, $original_width, $original_height);
        $image_save_function($new_image, $mini_path);

        return true;
    }

    return false;
}

/**
 * Removes the EXIF data from the given image,
 * and writes a new image without it at the new
 * location.
 *
 * Thanks http://stackoverflow.com/a/38862429 .
 *
 * @param string $old Path to the old image.
 * @param string $new Path to the new image without EXIF data.
 */
function remove_exif($old, $new)
{
    $f1 = fopen($old, 'rb');
    $f2 = fopen($new, 'wb');

    // Find EXIF marker
    while (($s = fread($f1, 2))) {
        $word = unpack('ni', $s)['i'];
        if ($word == 0xFFE1) {
            // Read length (includes the word used for the length)
            $s = fread($f1, 2);
            $len = unpack('ni', $s)['i'];
            // Skip the EXIF info
            fread($f1, $len - 2);
            break;
        } else {
            fwrite($f2, $s, 2);
        }
    }

    // Write the rest of the file
    while (($s = fread($f1, 4096))) {
        fwrite($f2, $s, strlen($s));
    }

    fclose($f1);
    fclose($f2);
}

function random_string($str_length = 10)
{
    $keyspace = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $len = strlen($keyspace);
    $random_string = '';

    for ($i = 0; $i < $str_length; $i++)
    {
        $random_string .= $keyspace[rand(0, $len - 1)];
    }

    return $random_string;
}

/**
* A sweet interval formatting, will use the two biggest interval parts.
* On small intervals, you get minutes and seconds.
* On big intervals, you get months and days.
* Only the two biggest parts are used.
*
* @param DateTime $start
* @param DateTime|null $end
* @return string
*/
function format_date_diff($start, $end=null) {
    if(!($start instanceof DateTime))
    {
        $start = new DateTime($start);
    }

    if($end === null)
    {
        $end = new DateTime();
    }

    if(!($end instanceof DateTime))
    {
        $end = new DateTime($start);
    }

    $interval = $end->diff($start);
    $doPlural = function($nb,$str){return $nb>1?$str.'s':$str;}; // adds plurals

    $format = array();
    if($interval->y !== 0)
    {
        $format[] = "%y ".$doPlural($interval->y, "year");
    }
    if($interval->m !== 0)
    {
        $format[] = "%m ".$doPlural($interval->m, "month");
    }
    if($interval->d !== 0)
    {
        $format[] = "%d ".$doPlural($interval->d, "day");
    }
    if($interval->h !== 0)
    {
        $format[] = "%h ".$doPlural($interval->h, "hour");
    }
    if($interval->i !== 0)
    {
        $format[] = "%i ".$doPlural($interval->i, "minute");
    }

    if($interval->s !== 0)
    {
        if(!count($format))
        {
            return "less than a minute ago";
        }
        else
        {
            $format[] = "%s ".$doPlural($interval->s, "second");
        }
    }

    // We use the two biggest parts
    if(count($format) > 1)
    {
        $format = array_shift($format)." and ".array_shift($format);
    }
    else
    {
        $format = array_pop($format);
    }

    return $interval->format($format);
}
