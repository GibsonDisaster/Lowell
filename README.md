# Lowell Programming Language

### About
Out of the ashes of the [Carrie Programming Language](https://github.com/HenningTonko/Carrie-Programming-Language) rises the Lowell Programming Language. When I started designing and writing Lowell I first went to Carrie to see where I felt like I could improve:
* Terrible job parsing variable declarations, leading to ugly solution
* Functions required type notation for args, but never did any checking
* You can't assign a variable to the result of a function, opting instead for the lazy and terrible "bind" function
* Comments were incredibly finnicky to use
* "{}" couldn't be used to denote function blocks
* No custom data types

With these failings in mind, I set out to create a better programming language focusing on strong typing and functional programming. Still very early in development and with no compiler in sight, this is just a parser for the time being, but will eventually have code gen implemented in some capacity.

### Dependencies
[Haskell](www.haskell.org) - Parser is written in Haskell
[Parsec](http://hackage.haskell.org/package/parsec)