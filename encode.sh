#! /bin/bash

encode=$(file -i $1 | awk -F '=' '{print $2}')
file="clean_$1"
echo $encode
iconv -f $encode -t utf8 -c $1 | sed -e 's/&#x/\\u/g' | sed -r 's/;//g' > $file

