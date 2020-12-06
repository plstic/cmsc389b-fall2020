@echo off
REM setlocal enabledelayedexpansion
echo *** Starting vm
set vol=%1
if [%vol%] == [] (
    set vol=src
)
set vol="%cd%"\%vol%
echo *** Remember to "exit" the terminal instead of closing the window
docker run -it --rm -v %vol%:/home/student/src -p 65001:65001 cmsc389b:intercal
echo *** Finished -- make sure the vm was stopped/deleted
pause
