# bf-interpreter
An interpreter for BF written in Racket

## Description
This is an interpreter for the esoteric language [BF](https://en.wikipedia.org/wiki/Brainfuck).
This particular implementation retains the ascii-only restriction, and uses invalid text as comments
(although in some other implementations, invalid text marks the end of a line).  
This program can be run in [Racket](http://racket-lang.org/), which is a dialect of Lisp.
There should be little issue converting the program to other dialects.
