## Fancydiff

Fancydiff is a a diff coloring wrapper for Git that supports both intra-line diffs, and source code syntax highlighting.

<img src="doc/fancydiff-example.png">

This similar to what you see on Github, but instead it is done in the user's console.

## Dependencies

This can be built with the Haskell community's 'stack' tool. Dependencies are entirely in Stackage.

## Limitations.

 * You need to pass --full-index to `git diff`, `git log`, and `git show`.
 * Too few source code languages are supported.
 * It is slow than regular diff highlighting, because it needs to do full source highlighting for every changed file in the diff. However, depending on the
usage pattern, it may not be noticable.
