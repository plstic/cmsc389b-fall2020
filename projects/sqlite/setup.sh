sudo docker build --tag cmsc389b-sqlite .
sudo docker run -it --rm -v ./:/src cmsc389b-sqlite
