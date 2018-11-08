rec {

    bootstrap = import <nixpkgs> {};

    # NixOS unstable, 2018 Nov 8
    pin.rev = "179b8146e668636fe59ef7663a6c8cd15d00db7e";

    # nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz
    pin.sha256 = "0fjab831i12lsnizvviz9f7k6dmi2gpvkysawc8r6nv0naa2q5fh";

    nixpkgs = bootstrap.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        inherit (pin) rev sha256;
    };

    pkgs = import nixpkgs {};

    haskell = pkgs.haskell.packages.ghc844.ghcWithPackages (p: [ p.foundation ]);

    cabal = pkgs.haskellPackages.cabal-install;

}
