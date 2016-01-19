## Fancydiff

Fancydiff is a diff coloring wrapper for Git that supports both intra-line diffs, and source code syntax highlighting.

<img src="doc/fancydiff-example.png">

This similar to what you see on Github, but instead it is done in the user's console.

## Dependencies

This can be built with the Haskell community's 'stack' tool. Dependencies are entirely in Stackage.

## Setup

With `fancydiff` in `$PATH` these aliases can be used in `.gitconfig`:

```
[alias]
    log-fancy = "!git -c color.diff=off -c pager.log='fancydiff | less' log $@ || true"
    show-fancy = "!git -c color.diff=off -c pager.show='fancydiff | less' show $@ || true"
```

For it to function, you need to do either of these things:
 * Configure `color.diff = off`, and `pager.diff = fancydiff | less`. Same for `log` and `show`.
 OR:
 * Pipe it via `fancydiff`.
 OR:
 * Pass `-c color.diff=off` between `git` and its command, and pipe it via `fancydiff`.

## Limitations

 * Does not work with un-added modifications (diff to index).
 * Too few source code languages are supported.
 * It is slow than regular diff highlighting, because it needs to do full source
   highlighting for every changed file in the diff. However, depending on the
   usage pattern, it may not be noticable.
