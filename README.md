#### About & Why
==============
Beamwhale is an erlang library to replicate what docker does at a tiny scale.
My goal for this project was to be able to list available tags for an image.
Pull image to a local repository. And use the image to create an isolated resource (container)


Why? well to gain more knowledge in erlang and linux at the same time :)
I am well aware that erlang isn't supposed to be used for these kind of tasks..

What I also realized among the way is that, I tried to develop and use this functionality
with my standart user, but creating containers require superuser privileges, and i believe
the way docker handles it is that they create a special user group with permissions and add your user to it so that you don't need superuser when using docker.

One simple example is that the unshare call (<http://man7.org/linux/man-pages/man2/unshare.2.html>) will always fail for me in ubuntu 16.04 if i don't acquire superuser. Another example is that if you opt-in for building the root filesystem with overlayfs you again need superuser privileges.

> get tags of a docker container image

> (any available library on docker public registry)

[![asciicast](https://asciinema.org/a/91e1psjachs5vd48qsep68jev.png)](https://asciinema.org/a/91e1psjachs5vd48qsep68jev)


> pull the container image with the tag you would like

> you can also choose to opt-out of providing a tag

> in that case it will default to latest tag

[![asciicast](https://asciinema.org/a/clhrxyetoaqetr97f24y9qwxz.png)](https://asciinema.org/a/clhrxyetoaqetr97f24y9qwxz)



> create and run a command in a container

> with the image you would choose

[to be populated]
