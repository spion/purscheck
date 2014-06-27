# purscheck

A flycheck-compatible wrapper of the purescript compiler

## setup

1. Build purscheck
2. Add purscheck.el to your emacs

## how it works

Add a file called `.purescript-paths` in your project directory. This file should
contain a list of globs that reference all the relevant source files. 

Example:

    bower_components/**/*.purs
    src/**/*.purs

# license

MIT
