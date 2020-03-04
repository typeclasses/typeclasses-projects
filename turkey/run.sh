#!/usr/bin/env bash

nix-shell -p 'haskellPackages.ghcWithPackages (p: [p.diagrams])' \
          --run "ghcid --command='ghci turkey.hs' --test=:main"
