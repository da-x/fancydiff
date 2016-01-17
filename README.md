## Fancydiff

Fancydiff is a a diff coloring wrapper for Git that supports both intra-line diffs, and source code syntax highlighting.

<img src="doc/fancydiff-example.png">

This similar to what you see on Github, but instead it is done in the user's console.

## Dependencies

This can be built with the Haskell community's 'stack' tool. Dependencies are entirely in Stackage.

## Setup

For it to function, you need to do either of these things:
 * Pass `--full-index` to `git diff`, `git log`, and `git show`.
 OR:
 * Pass `-c core.abbrev=40 -c color.diff=off` between `git` and its command.
 OR:
 * Configure `core.abbrev = 40`, `color.diff = off`, and `pager.diff = fancydiff | less`. Same for `log` and `show`.

## Limitations

 * Does not work with un-added modifications (diff to index).
 * Too few source code languages are supported.
 * It is slow than regular diff highlighting, because it needs to do full source highlighting for every changed file in the diff. However, depending on the
usage pattern, it may not be noticable.
