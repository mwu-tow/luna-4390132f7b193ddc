# Luna Studio

<img src="https://github.com/luna/luna-studio/raw/master/resources/logo.ico" align="right" style="margin: 0 auto;">

### Visual and textual functional programming language with a focus on productivity, collaboration and development ergonomics.

Luna is a developer’s whiteboard on steroids. Design, prototype, develop and refactor any application simply by connecting visual elements together. Collaborate with co-workers, interactively fine tune parameters, inspect the results and visually profile the performance in real-time.

Visit www.luna-lang.org to learn more!



## Quick installation

The easiest way of installing Luna Studio is by using the graphical Luna Installer, which can be downloaded using the following links:
* Mac OS: http://packages.luna-lang.org/darwin/lunaInstaller.zip
* Linux: http://packages.luna-lang.org/linux/lunaInstaller.zip
* Windows: http://packages.luna-lang.org/windows/lunaInstaller.zip

All you need to do is download and extract the zip and run the application. It will then guide you through the installation process.

## Using Luna Studio

For a tutorial, documentation, explanation of the ideas behind Luna please visit http://docs.luna-lang.org. It is available in a form of a book, to provide a gentle and fairly complete introduction to the application as well as the new visual-textual paradigm.

## Bootstrapping for development

However, if you want to develop Luna Studio, the installation process is a bit more involved. First of all, you will need the [Luna Manager](https://github.com/luna/luna-manager). The instructions for obtaining it are inside [its repository](https://github.com/luna/luna-manager).

NOTE: even though Luna Studio does support Windows as one of the target operating systems, it is not possible to develop the frontend part on Windows. This is dictated by the GHCJS project not targeting Windows right now.

The prerequisites for building Luna Studio are the following (with Ubuntu apt package names in the parentheses for convenience):
* wget
* gcc, g++, make (build-essential)
* libffi (libffi-dev)
* libgmp (libgmp-dev)
* xz-utils
* zlib (zlib1g-dev)
* git
* gnupg
* ZeroMQ >= 4 (which, interestingly, is libzmq3-dev on Ubuntu)
* libpng (libpng-dev)
* openssl
* SetupTools for Python (python-setuptools)
* libssl (libssl-dev)
* libtinfo (libtinfo-dev)
* pkg-config
* alex
* PIP for Python (python-pip)

Once you make sure you have that installed, you can proceed to the environment setup. Assume you already have the `luna-manager` binary on your system, the command is the following:
```
$ luna-manager develop luna-studio
```
This will install Luna Studio along with some dependencies (including the Haskell ecosystem) to `$HOME/luna-develop/apps/luna-studio`. If you wish to install to a different location, please supply an additional `--path` argument to the `develop` command. From now on we will refer to this location as `$LUNA_STUDIO_REPO`.

Once the `develop` command finishes, you are ready to start developing Luna Studio. To rebuild the whole application, use:
```
$ ./build
```
You can choose to rebuild only the backend or frontend part, by passing an additional `--backend` or `--frontend` flag, respectively.

To run the application in developer mode, use:
```
$ $LUNA_STUDIO_REPO/dist/bin/main/luna-studio --develop
```
  
## Troubleshooting
  * If anywhere during your development process you will encounter errors related to missing files (caused by accidental removal of a dependency, for example), you can try running: `luna-manager develop luna-studio --path $LUNA_STUDIO_REPO --download-dependencies`.
    
* [Linux] App image requires FUSE to run. While most Linux distributions have it pre-installed, it can be missing in your specific case. If your distro happens to lack FUSE, you can either install it or mount the AppImage yourself with:
    ```
    sudo mount -o loop Some.AppImage /mnt
    /mnt/AppRun
    ```
