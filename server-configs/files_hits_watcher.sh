#!/bin/bash

# For nginx log format

tail -Fn0 $1 | \
while read line ; do
        echo "$line" | grep -E "GET \/(mini_)?([0-9]+)\\.(jpg|jpeg|gif|bmp|ico|png|BMP|JPG|JPEG|PNG|SVG|svg)"
        if [ $? = 0 ]
        then
            php /home/www/i.zcraft.fr/console i:hit --quiet $(echo "$line" | cut -d '"' -f2 | cut -d ' ' -f2)
        fi
done
