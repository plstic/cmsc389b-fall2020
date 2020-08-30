#!/bin/sh
if [ -z "$1" ] ; then
  shared=$PWD/src
else
  shared=$PWD/$1
fi
sudo docker run -it --rm -e DISPLAY=$DISPLAY -v $shared:/home/student/ -p 65001:65001 cmsc389b
