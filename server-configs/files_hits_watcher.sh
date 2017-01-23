#!/bin/bash

tail -Fn0 logfile | \
while read line ; do
        echo "$line" | grep -E "GET \/(.+)\\.(jpg|jpeg|gif|bmp|ico|png|BMP|JPG|JPEG|PNG|SVG|svg)"
        if [ $? = 0 ]
        then
            php /home/www/i.zcraft.fr/console i:hit --quiet $(echo "$line" | cut -d '"' -f2 | cut -d ' ' -f2)
        fi
done
