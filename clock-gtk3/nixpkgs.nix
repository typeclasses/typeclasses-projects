# nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz

(import <nixpkgs> { }).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";

  # NixOS unstable, 2018 Nov 17
  rev = "80738ed9dc0ce48d7796baed5364eef8072c794d";
  sha256 = "0anmvr6b47gbbyl9v2fn86mfkcwgpbd5lf0yf3drgm8pbv57c1dc";

}
