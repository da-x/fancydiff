# Changelog

## Unreleased

- Fix an exception occuring when Sub-module changes are in the history.
- Support for JavaScript syntax highlighting.
- Color adjustments for bright theme.

## 0.2.1

Bug fixes:

- Reading file blob content from Git that contains invalid UTF-8
  should not resulted in an exception.
- Small Haskell highlighting fixes.

## 0.2.0

Bug fixes:

- Haskell syntax highlighting received a face lift. Now looking
  much fancier.

Features:

- Added a 'setup' command for easy manipulation of Git configuration
  for installing Fancydiff. Supports both local and global Git configs, 
  via pager overrides, or command aliases.

  For a quick start, just run 'fancydiff setup --global' after 
  upgrading.

- Support for bright terminals (e.g. white backgrounds). Now there are 
  two builtin themes: dark and bright.
- HTML generation, both for inline and CSS. Can be used as 
  a Haskell library for generating syntax highlighting in blogging
  platforms such as Hakyll. The exported interfaces from the Library 
  are experimental.
  See [example](http://blog.aloni.org/posts/st-monad-perf-with-exceptions/).
- Fancydiff is sporting a new command line interface, exposing 
  more functionality, including a builtin method to invoke a pager
  (currently only `less` is supported). Old command line interface
  is not compatible with the new one.

## 0.1.3

Bug fixes:

- Fix handling of submodules appearing in diffs
- Theme should be insensitive to terminal's default FG color,
  which means that even if it is not white or grayish, coloring
  looks consistent among users.

## 0.1.2

Bug fixes:

- Fix highlighting of the 'Merge:' line in merge commits.
- Packaging for Mac OS x includes the shared libraries
  that Fancydiff depends on.

Other changes:

- Packaging for Linux - Fedora / Ubuntu
- Packaging for Mac OS X
