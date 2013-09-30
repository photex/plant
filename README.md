What is Plant?
==============

Plant is a sketch of how I'd like to interact with Common Lisp projects. The motivation
to actually write it cam from [Lisp In Summer Projects](http://lispinsummerprojects.org/)
and once I had something that more or less met my initial use case I quickly realized
that I something that my life a bit more pleasant.

At the moment, Plant is little more than a simple script that follows a couple of
conventions I've become accustomed to. It makes it very very easy for me to work on
a project on multiple machines with different lisps. This has been particularly useful
when working on [cl-sdl2](http://github.com/lispgames/cl-sdl2). It's also pretty *nix
specific at the moment.

What do you mean "plant"?
-------------------------

For me, a lisp is like a garden and the act of development is a process of helping things
grow. I thought about what words could be used to describe this and *plant* just sort of
stuck.

However, it's a loaded word and I think everyone could interpret it differently. If
you think of a cool recursive ancronym then I'll totally adopt that instead.


Why is Plant?
=============

The foundations for Plant are [Quicklisp](quicklisp.org) and [ASDF](http://common-lisp.net/project/asdf/).
I am aware of [clbuild](http://common-lisp.net/project/clbuild/) and Plant is more or
less a sort of clbuild clone written in Scheme instead of bash.

Scheme?
-------

Yeah. Specifically [Guile](http://www.gnu.org/software/guile).

In order for this to actually count as an entry for *Lisp In Summer Projects* it
had to be written in lisp. As someone who is relatively new to lisp in general, this
gave me a chance to stretch out and experience a highly regarded scheme implementation
that is widely available or easy to build or install on modern systems. 

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

