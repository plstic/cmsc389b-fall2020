# Docker

For installation, see [docker.md](../guides/docker.md)

Docker environment instructions. Docker has wonderful documentation that we highly encourage you to read [here](https://docs.docker.com/) if you are still confused. If you are still lost, reach out to us.

## Linux

1. Run [`build.sh`](build.sh)
   * _Be patient while it builds_
   * This should build a new Docker image based on our `Dockerfile`, and tag it
   `cmsc389b`
2. Run [`run.sh`](run.sh), which will create and run a container based on the 
`cmsc389b` image

Docker is like a VM and you can have a shared drive between your docker 
container and your host system. The default location after running the `run.sh`
script is `./src` however you can supply a path to whichever folder you want as 
a command line argument. 

## Windows

1. Run [`build.bat`](build.bat)
   * _Be patient while it builds_
   * This should build a new Docker image based on our `Dockerfile`, and tag it `cmsc389b`
1. Run [`run.bat`](run.bat), which will create and run a container based on the `cmsc389b` image

**Note**: if you start your Docker container using Docker Desktop, then your shell will be `/bin/sh`. If you do not want this, run `/bin/bash` in the open terminal window, or just use `run.bat`.

## macOS

Follow Windows instructions, but run the linux scripts instead.
