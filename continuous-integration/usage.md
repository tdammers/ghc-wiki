# Maintaining CircleCI



Most of the configuration of GHC's CircleCI instance can be found in one of three places:


- `.circleci/config.yml` in the GHC tree defines the set of jobs to be run and when, and the build procedure
- the Docker images in `.circleci/images/*` define the initial build environments
- `.circleci/prepare-system.sh`, which is run at the beginning of every build, does a bit more setup (some of this probably ought to be moved into the Docker files)

## Updating the Docker images



To update one of the Docker images you will need to install the Docker engine. To build the new image (using `x86_64-linux-fedora` as a example),


```
$ docker build .circleci/images/x86_64-linux-fedora
# will produce a fair amount of output...
Successfully built b6f13cfca0b4
```


Now that we have built the image we need to give it a name and upload it to Docker Hub, where it will be found by CircleCI (updating the version number as appropriate):


```
$ docker tag b6f13cfca0b4 ghcci/x86_64-linux-fedora:0.0.4
$ docker push ghcci/x86_64-linux-fedora:0.0.4
```


Finally, `.circleci/config.yml` needs to be updated to point to the new image version. Specifically, change `docker.image` of the `"validate-x86_64-fedora"` to `ghcci/x86_64-linux-fedora:0.0.4`. Now new builds should use your new image.


