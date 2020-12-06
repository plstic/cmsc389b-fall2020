@echo off
echo *** Building image
docker build --tag cmsc389b:intercal .
echo *** Finished
pause
