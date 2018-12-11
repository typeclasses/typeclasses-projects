#!/usr/bin/env bash

set -eu

nix run \
    --file default.nix \
    haskell \
    -c runhaskell Main.hs
