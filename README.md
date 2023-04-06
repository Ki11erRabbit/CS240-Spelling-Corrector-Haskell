# CS240-Spelling-Corrector-Haskell
BYU CS 240 Spelling Corrector but written in Haskell. Good luck trying to cheat with this

This is a project that I made in order to learn Haskell as well as the functional programming paradigm.
The original Spelling Corrector was written in Java which is an object oriented language. I did rewrite this before in Rust.


# How to use
- You need to install the Haskell toolchain as well as a Haskell compiler. The makefile I wrote only works with ghc.
- Then it is as simple as running `ghc Main.hs`
  - *OR* if you are on MacOS, Linux, or BSD then you can just run `make` to build the program

## Usage
The options for the program are as follows
- `--help` displays the help for the program
- An optional argument for the dictionary file with words separated by whitespace
- An optional parameter for the word to be corrected

You can also run the program in the ghci repl by doing this:

```
ghci
ghci> :l Main.hs
ghci> main
```


### Examples
`spelling-corrector`

`spelling-corrector --help`

`spelling-corrector notsobig.txt`

`spelling-corrector notsobig.txt test`
