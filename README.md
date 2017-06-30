# budapest-fp-meetup

## Running the scala example

You can `run` the sample main from sbt:

```
$ cd scala
$ sbt
> run "(((lambda (x) (lambda (y) x)) 3) 4)"
```

## References:

Matthias Fleissen et al., Beyond Continuations, 1987. Technical Report, Indiana University
http://www.ccs.neu.edu/racket/pubs/felleisen-beyond.pdf

Jonathan Tang, Write yourself a scheme in 48 hours, 
https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

Matt Might, Writing CEK-style interpreters (or semantics) in Haskell, 
http://matt.might.net/articles/cek-machines/ 
