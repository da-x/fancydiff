## Fancydiff

[![Build Status](https://travis-ci.org/da-x/fancydiff.svg?branch=master)](https://travis-ci.org/da-x/fancydiff)

Fancydiff is a diff coloring wrapper for Git, under Linux or MacOS X, that supports both intra-line diffs, and source code syntax highlighting.

<img src="doc/fancydiff-example.png">

This similar to what you see on Github, but instead it is done in the user's console.

### Download and install

#### Linux

First make sure that you have a terminal program that supports 24 bit True Color (see this [gist](https://gist.github.com/XVilka/8346728)).

Ubuntu users of URxvt can install from [my build of urxvt on Launchpad](https://launchpad.net/~alonid/+archive/ubuntu/rxvt-console-24bit-color),

Fedora/CentOS users can use my patched builds of URxvt installable from [Copr](https://copr.fedorainfracloud.org/coprs/alonid/rxvt-unicode-24bit-ansi-color/).

Latest binaries of Fancydiff can be installed on major distributions.

On **Fedora** 22 onwards:

```
sudo dnf copr enable alonid/fancydiff
sudo dnf install fancydiff
```

For EPEL/CentOS/Red Hat 7, visit [Copr](https://copr.fedorainfracloud.org/coprs/alonid/fancydiff/).

On **Ubuntu**:

```
sudo add-apt-repository ppa:alonid/fancydiff
sudo apt-get update
sudo apt-get install fancydiff
```

You can also visit [Fancydiff's PPA in Launchpad](https://launchpad.net/~alonid/+archive/ubuntu/fancydiff).

#### MacOS X

Download a **test release or a nightly** of [iTerm2](https://www.iterm2.com/downloads.html), which supports 24-Bit True Color
ANSI codes. Then, do the following:

```
brew install https://raw.githubusercontent.com/da-x/fancydiff/brew-lastest/fancydiff.rb
```

### Or, compile from source

Follow the [building instructions](doc/compilation-from-source.md).

## Git configuration

With `fancydiff` in `$PATH` you can use the `setup` command to modify the Git configuration.

```
fancydiff setup [--local] [--aliases]
```

More details [here](doc/git-configuration.md) about how the Git configuration is modified
to enable Fancydiff.

## Limitations

 * Too few source code languages are supported (currently: C/C++, Haskell,
   Python, Java, JavaScript, Go).
 * Some small original coloring features from Git itself are missing.
 * Some special Git modes don't use the pager (e.g. `git checkout -p`, 
   in which Fancydiff is not presently activated.
 * 3-way diff is not yet supported.

## Contribution and bug reports

Everyone is welcome to contribute and report issues here via Github!
