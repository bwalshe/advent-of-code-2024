# Advent of Code 2024

_Last Christmass I tried to do AoC in Haskell and document the process,_ 
_but I ended up giving up after a few days._
_This year, to save me from tears,_
_I'm just going to solve the problems without discussing it_

This is the reop where I'm keeping my [AoC](https://adventofcode.com/) 
solutions. Last time I tried making a big thing out of it, using idiomatic
Haskell and documenting my progress each day - what difficulties did I face and
what did I learn, etc. I made it about 8 days in before I got bored and gave 
up. This time I'm just going to focus on getting the problems done. I don't 
need to have good Haskell, I only use it once a year. 


## Some thoughts about the last year in Haskell
1. I literally did not look at any Haskell since last year.
2. I couldn't understand any of the code I had written last year.
3. It does seem that `OverloadedStrings` behaves more sanely now.
4. My biggest problem with getting things done this year is trying
   to write parsers. This is probably because I am using a parsing
   library instead of just reading in lines, and it is massive 
   overkill.


```

         |
        -+-
         A
        /=\               /\  /\    ___  _ __  _ __ __    __
      i/ O \i            /  \/  \  / _ \| '__|| '__|\ \  / /
      /=====\           / /\  /\ \|  __/| |   | |    \ \/ /
      /  i  \           \ \ \/ / / \___/|_|   |_|     \  /
    i/ O * O \i                                       / /
    /=========\        __  __                        /_/    _
    /  *   *  \        \ \/ /        /\  /\    __ _  ____  | |
  i/ O   i   O \i       \  /   __   /  \/  \  / _` |/ ___\ |_|
  /=============\       /  \  |__| / /\  /\ \| (_| |\___ \  _
  /  O   i   O  \      /_/\_\      \ \ \/ / / \__,_|\____/ |_|
i/ *   O   O   * \i
/=================\
       |___|
```
