#!/usr/bin/env bash

set -eu

nix run \
    --ignore-environment \
    --keep LANG \
    --keep LOCALE_ARCHIVE \
    --keep TERM \
    --file default.nix \
    haskell ghcid \
    -c ghcid Main.hs
