What is Plant?
==============

Plant is a sketch of how I'd like to interact with Common Lisp projects. The motivation
to actually write it came from [Lisp In Summer Projects](http://lispinsummerprojects.org/).

Happily, once my initial use-case was met it was useful enough to encourage further
development. It's a pretty good start for a tool that could become helpful to others.

At the moment, Plant is little more than a simple script that follows a couple of
conventions I've become accustomed to. It makes it very very easy for me to work on
a project on multiple machines and with different lisps. Currently it only knows how
to work with Clozure and SBCL but I'm keen to add support for other Common Lisp implementations.
This has been particularly useful when working on [cl-sdl2](http://github.com/lispgames/cl-sdl2).
It's also pretty *nix specific at the moment.

Examples
--------

You can find an example project [here](http://github.com/photex/plant-example).

**TL;DR** The most basic interaction with plant could be:
- `mkdir new-project`
- `cd new-project`
- `plant swank`

This will create a new plant project with some standard quickloads already loaded into the lisp,
load the REPL, and then run swank on the default port of 4005. From now on, anytime you enter
this project directory and run `plant run` or `plant swank`, the lisp will have already been setup
and you'll be able to quickly get to the fun stuff.

What do you mean "plant"?
-------------------------

For me, a lisp is like a garden and the act of development is a process of helping things
grow. I thought about what words could be used to describe this and *plant* just sort of
stuck.

However, it's a loaded word and I think everyone could interpret it differently. If
you think of a cool recursive ancronym then I'll totally adopt that instead.


Why is Plant?
=============

The foundations for Plant are [Quicklisp](http://quicklisp.org) and [ASDF](http://common-lisp.net/project/asdf/).
I am aware of [clbuild](http://common-lisp.net/project/clbuild/) and Plant is more or
less a sort of clbuild clone written in Scheme instead of bash.

Scheme?
-------

Yeah. Specifically [Guile](http://www.gnu.org/software/guile).

In order for this to actually count as an entry for *Lisp In Summer Projects* it
had to be written in lisp. As someone who is relatively new to lisp in general, this
gave me a chance to stretch out and experience a highly regarded scheme implementation
that is widely available or easy to build or install on modern systems.

It is my goal to re-write Plant totally as a Common Lisp tool. Not because of any shortcomings
with Scheme but simply because it's a tool for Common Lisp development that should really be
leveraging the work put into Quicklisp and ASDF3.

Inspiration
------------

[Leiningen](https://github.com/technomancy/leiningen) is a big inspiration for Plant.
It's certainly more mature and well thought out than Plant is. It's also built on a
pretty great foundation for establishing solid repeatable builds of projects and
libraries.

Like almost every modern JVM based build tool or project system, dependencies are handled
by Ivy/Maven repositories. Say what you must about the amount XML and ceremony associated
with producing artifacts for these systems; for the end user or developer it all works
brilliantly.

I believe that it's not out of our reach to have a similar tool for Common Lisp projects.


How do I Plant?
================

Getting started with plant shouldn't be too difficult but it's not automated.

Prerequisites
--------------

Plant requires [Guile](http://www.gnu.org/software/guile) 2.x. and either
[SBCL](http://sbcl.org) or [Clozure](http://ccl.clozure.com):
- OSX: `brew install guile --devel && brew install sbcl ccl rlwrap`
- Ubuntu: `sudo apt-get install guile2 sbcl rlwrap`
- ???: If your package manager doesn't have Guile 2.x then you should build it from source.
Clozure, and SBCL are available for a wide variety of platforms from their websites.

Grab plant
-----------

Here is how I make plant available in my shell:
- `cd $HOME`
- `git clone git://github.com/photex/plant.git .plant`
- `cd ~/bin`
- `ln -s ~/.plant/plant`

Plant can be placed anywhere. The primary plant script just needs to be in your path,
and the environment variable **$PLANT_HOME** must be set if plant isn't installed to
`~/.plant/`.

You can periodically check for updates using git:
- `git fetch`
- `git merge origin/master`

Start using plant
-----------------

You should now be good to go. The easiest way to mess around with plant is to make
a folder and start trying out the basic workflow:
- `cd ~/Projects`
- `mkdir test-project`
- `cd test-project`
- `plant build`
- `plant run`

You should now be ready to hack! When *rlwrap* is found on **$PATH** it'll be used
automatically to make the interaction with the repl a bit easier.

You can control which lisp is used by setting the environment variable **$PLANT_LISP**.
At the moment your options are really only `sbcl`, `ccl`, or `ccl64`. I would very
much like to improve this in the future.

