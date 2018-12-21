#!/usr/bin/env bash

set -eu

nix run \
    --ignore-environment \
    --keep LANG \
    --keep LOCALE_ARCHIVE \
    --keep TERM \
    --keep HOME \
    --file default.nix \
    haskell \
    -c \
    ghci Main.hs
