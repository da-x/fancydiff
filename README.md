## Fancydiff

[![Build Status](https://travis-ci.org/da-x/fancydiff.svg?branch=master)](https://travis-ci.org/da-x/fancydiff)

Fancydiff is a diff coloring wrapper for Git, under Linux or MacOS X, that supports both intra-line diffs, and source code syntax highlighting.

<img src="doc/fancydiff-example.png">

This similar to what you see on Github, but instead it is done in the user's console.


## Git configuration

With `fancydiff` in `$PATH` the following configuration can be used in `.gitconfig`. It can be a drop-in
replacement for `diff-highlight` and `git`'s own diff coloring, via:

```
[color]
    diff = off
[pager]
    log = fancydiff | LESSANSIENDCHARS=mK less
    show = fancydiff | LESSANSIENDCHARS=mK less
    diff = fancydiff | LESSANSIENDCHARS=mK less
```

Optionally, it may be used via aliases:

```
[alias]
    log-fancy = "!git -c color.diff=off -c pager.log='fancydiff | LESSANSIENDCHARS=mK less' log $@ || true"
    show-fancy = "!git -c color.diff=off -c pager.show='fancydiff | LESSANSIENDCHARS=mK less' show $@ || true"
    diff-fancy = "!git -c color.diff=off -c pager.diff='fancydiff | LESSANSIENDCHARS=mK less' diff $@ || true"
```


## Setup on Linux (Fedora-based, Ubuntu/Debian-based)

A terminal supporting TrueColor is currently required.

See this [gist](https://gist.github.com/XVilka/8346728).

### Binaries

On **Fedora** 22 onwards:

```
sudo dnf copr enable alonid/fancydiff
sudo dnf install fancydiff
```

For EPEL/CentOS/Red Hat 7, visit [Copr](https://copr.fedorainfracloud.org/coprs/alonid/fancydiff/).

On **Ubuntu** Wily:

```
sudo add-apt-repository ppa:alonid/fancydiff
sudo apt-get update
sudo apt-get install fancydiff
```

Otherwise visit the [PPA page](https://launchpad.net/~alonid/+archive/ubuntu/fancydiff).

### Build from source

#### Dependencies

For Fedora/CentOS, do `sudo yum install libicu-devel zlib-devel openssl-devel gmp-devel pcre-devel`
For Debian/Ubuntu, do `sudo apt-get install libicu-dev libz-dev libssl-dev libgmp-dev`
Download and install [haskell-stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html) for Linux,


#### Build

In the Git clone of Fancydiff, do the following:

```
stack setup
stack install
```


## Setup on MacOS X

### Dependencies

Download a **test release or a nightly** of [iTerm2](https://www.iterm2.com/downloads.html), which supports 24-Bit True Color
ANSI codes.

Download [haskell-stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html#mac-os-x) for MacOS X,
and do the following:

```
brew install icu4c
brew install openssl
```

### Build and install

Change to the cloned repo of Fancydiff and do the following:

```
stack setup
stack install \
    --extra-include-dirs=/usr/local/opt/icu4c/include   \
    --extra-lib-dirs=/usr/local/opt/icu4c/lib           \
    --extra-include-dirs=/usr/local/opt/openssl/include \
    --extra-lib-dirs=/usr/local/opt/openssl/lib
```

## Limitations

 * Too few source code languages are supported.
 * Some small original coloring features from Git itself are missing.
