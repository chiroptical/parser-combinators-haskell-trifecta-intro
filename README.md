Baby's First Parser Combinators w/ Haskell and Trifecta
---

My first haskell presentation ever! Please be nice...

This presentation comes from working through Chapter 24 of [Haskell Programming
from First Principles](http://haskellbook.com/). It was presented at the
[Pittsburgh Functional Programming
Meetup](https://www.meetup.com/Pittsburgh-Functional-Programming-Meetup/events/gctsjlyzjbnb/)
on June 10, 2019. It is meant as a quick getting started guide to parsing in
Haskell using Trifecta. I worked on this chapter on [my twitch
channel](https://twitch.tv/chiroptical) where we attempted to use the
Applicative Functor context instead of the Monadic context where possible.

## Requirements

On Pop!_OS 19.04 (and most Ubuntu based systems),

```
sudo apt install texlive texlive-latex-extra texlive-fonts-extra python3-pygments
```

## Compilation

```
pdflatex -shell-escape parsing.tex
```

## Example Parsers

All of the below are functional `stack` projects
which I used for the presentation. 

- `code/rational`
- `code/semanticVersions` 
- `code/ipv4Address`
- `code/phoneNumber`
