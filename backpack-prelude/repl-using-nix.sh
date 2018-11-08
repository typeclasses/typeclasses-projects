#! /usr/bin/env bash

nix run -f default.nix haskell cabal -c \
    cabal v2-repl --repl-options=-ignore-dot-ghci
