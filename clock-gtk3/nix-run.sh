#!/usr/bin/env bash

set -eu

nix run \
    --ignore-environment \
    --keep DISPLAY \
    --keep XAUTHORITY \
    --file default.nix \
    clock \
    -c clock
