docker build --tag cmsc389b-sqlite .
docker run -it --rm -v "%cd%":/src cmsc389b-sqlite
