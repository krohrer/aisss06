Project for the Advanced Image Synthesis Course at ETHZ, Summer 06.

          ______                                           _______     _____
  _____  /____/|    Procedural Buildings                  /     /|    /___/|
 /___/| |    | |    Last updated : 11 Dec. 2012          /_____/ |  _|   | |
|   | |_|    | |                                        |     |  | /_|   | |
|   |/__|    | |    Author : Kaspar Rohrer              |     | / |  |   | |
|___|___|____|/     E-mail : kaspar.rohrer@gmail.com    |_____|/  |__|___|/
______________________________________________________________________________
_  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  __  _  _  _  _  _  _  _  _  
______________________________________________________________________________

PREREQUISITES
-------------

You will need four things in order to compile this program:

ocaml           http://caml.inria.fr/
lablgl          https://forge.ocamlcore.org/projects/lablgl/
caml-inspect	https://github.com/krohrer/caml-inspect
ocaml-extlib	http://code.google.com/p/ocaml-extlib/
pa_monad	http://www.cas.mcmaster.ca/~carette/pa_monad/

If you have some kind of package management software installed on your
machine (e.g. apt-get on linux, or darwinports on a mac), some of
these packages might be readily available.

COMPILATION
-----------

After the above prerequisites have been satisfied, you may build the
program by simply calling `make` on the CLI. After successful
compilation, you can start the program by calling `./aisss06.native`
There are two build targets: The default one is 'aisss06.native', which is
compiled to machine code rather than to bytecode like
'aisss06.byte. Bytecode compilation is faster though and can be used to
debug the program.

KEYBINDINGS
-----------

ESC : exit application
i   : initialize geometry (again) with a different random seed

w, s    : move forwards / backwards
a, d    : strafe left / right
t, g    : look down / up
f, h    : look left / right
q, e    : move up / down
space   : smooth movements on / off (see also TIPS & TRICKS)

b   : blending on / off
l   : viewer lighting on / off
n   : draw outlines on / off
,   : draw geometry on / off
.   : draw spatial partition on / off
tab : toggle fullscreen

0..9    : select level of detail
[, ]    : switch between different scenes

TIPS & TRICKS
-------------

If you own a machine that is a lot faster than mine (which is well
possible), smooth movements might be just a little too responsive. To
get acceptable movements, try adjusting the factors on line 240 in
aisss06.ml. The whole player movements are basically just a big hack,
so please accept my apologies.

If you're on a mac, switch smooth movements off before taking
screenshots. Otherwise, you might get a snapshot where the image is
only partially rendered.
