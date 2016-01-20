## Fancydiff

Fancydiff is a diff coloring wrapper for Git that supports both intra-line diffs, and source code syntax highlighting.

<img src="doc/fancydiff-example.png">

This similar to what you see on Github, but instead it is done in the user's console.

## Dependencies

This can be built with the Haskell community's 'stack' tool.

## Setup

With `fancydiff` in `$PATH` the following configuration can be used in `.gitconfig`. It can be a drop-in
replacement for `diff-highlight` and `git`'s own diff coloring, via:

```
[color]
    diff = off
[pager]
    log = fancydiff | less
    show = fancydiff | less
    diff = fancydiff | less
```

Optionally, it may be used via aliases:

```
[alias]
    log-fancy = "!git -c color.diff=off -c pager.log='fancydiff | less' log $@ || true"
    show-fancy = "!git -c color.diff=off -c pager.show='fancydiff | less' show $@ || true"
    diff-fancy = "!git -c color.diff=off -c pager.show='fancydiff | less' diff $@ || true"
```

## Limitations

 * Too few source code languages are supported.
 * It is slow than regular diff highlighting, because it needs to do full source
   highlighting for every changed file in the diff. However, depending on the
   usage pattern, it may not be noticable.
