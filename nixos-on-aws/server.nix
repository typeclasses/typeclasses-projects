let
  nixos = import <nixpkgs/nixos> {
    configuration = import ./configuration.nix;
  };
in
  nixos.system
