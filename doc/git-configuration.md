## Git configuration

With `fancydiff` in `$PATH` the following configuration can be used in `.gitconfig`. It can be a drop-in
replacement for `diff-highlight` and `git`'s own diff coloring, via:

```
[color]
    diff = off
[pager]
    log = fancydiff stdin --pager=less
    show = fancydiff stdin --pager=less
    diff = fancydiff stdin --pager=less
```

Optionally, it may be used via aliases:

```
[alias]
    log-fancy = "!git -c color.diff=off -c pager.log='fancydiff stdin --pager=less' log $@"
    show-fancy = "!git -c color.diff=off -c pager.show='fancydiff stdin --pager=less' show $@"
    diff-fancy = "!git -c color.diff=off -c pager.diff='fancydiff stdin --pager=less' diff $@"
```
