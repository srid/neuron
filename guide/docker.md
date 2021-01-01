# Docker workflow

You can use neuron without installing it by trying the Docker image [sridca/neuron](https://hub.docker.com/r/sridca/neuron). The image is built automatically from the development version of neuron. 

You will need to [install Docker](https://docs.docker.com/get-docker/) if you do not already have it installed.

In order to quickly get started, try:

```bash
mkdir ~/zettelkasten
cd ~/zettelkasten
echo "hello world" > hello.md
touch neuron.dhall  # Mark this a neuron directory
docker run --rm -t -i -p 8080:8080 -v $(pwd):/notes sridca/neuron neuron gen -ws 0.0.0.0:8080
```

This will run the neuron server on the current directory which can be accessed at <http://localhost:8080>. The docker image operates on `/notes` as the current working directory, which is where you are expected to mount your notes directory.
