# Docker

Docker environment instructions. Docker has wonderful documentation that we highly encourage you to read [here](https://docs.docker.com/) if you are still confused. If you are still lost, reach out to us.

NOTE: we recommend, if you intend to keep this repository `git`-updated then you should copy the `docker/` directory somewhere else. You will keep your assignment code in `docker/src` to access it within the docker container.

## Linux

Standard Linux procedure. 
+ `sudo apt-get install docker`
  + or whatever other installation tool you use
+ Run [`build.sh`](build.sh)

Now you should have an image built which you should be able to use. You can run 
this by running the [`run.sh`](run.sh) script. Docker is like a VM and you can 
have a shared drive between your docker container and your host system. The 
default location after running this script is `./src` however you can supply a
path to whichever folder you want as a command line argument. 

## Windows

### Desktop version

1. [Install Docker Desktop](https://hub.docker.com/editions/community/docker-ce-desktop-windows/)
1. Run [`build.bat`](build.bat)
   * _Be patient while it builds_
   * This should build a new Docker image based on our `Dockerfile`, and tag it `cmsc389b`
1. Run [`run.bat`](run.bat), which will create and run a container based on the `cmsc389b` image

If you intend to `git pull` this repository again, you should keep your code somewhere other than the `docker/src` directory (unless you are confident in `git`; we do not intend to update the `docker/src` directory). To do this, copy the `docker/` directory somewhere you want to keep your code, and repeat the same process.

**Note**: if you start your Docker container using Docker Desktop, then your shell will be `/bin/sh`. If you do not want this, run `/bin/bash` in the open terminal window, or just use `run.bat`.

## macOS

Follow Windows instructions, but run the linux scripts instead.
