sicp-ans
========

My answer of SICP's exercise.

Creating this repo is to promote myself to insiting on reading and study this bible.

Mainly code developed by myself, and refer from SICP and guile & racket materials.

FAQ
===

### How to run it?

    racket [filename]

### What is displayln?

In racket, this is a internal function. But for some other intepreter, maybe not exist this func.

So simply

    (define (displayln x) (display x) (newline)).

I had run code with `racket -f [filename]`. This option `-f` forbid default output, so I use `displayln`.

However, find it is useless when run with `racket [filename]`. Still keep it for some old code.

### Why you display the answer, it seem not useful?

eh..

Becuase I am a vimer, and only using a simple vim script to call intepreter to debug.

REPL, slimv and emcas is too complex to me for now.
