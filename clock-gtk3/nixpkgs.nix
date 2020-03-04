# nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz

(import <nixpkgs> { }).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";

  # NixOS unstable, 2019 Feb 14
  rev = "36f316007494c388df1fec434c1e658542e3c3cc";
  sha256 = "1w1dg9ankgi59r2mh0jilccz5c4gv30a6q1k6kv2sn8vfjazwp9k";

}
