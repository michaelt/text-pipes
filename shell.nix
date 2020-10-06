let sources = import nix/sources.nix;
in
{ pkgs ? import sources.nixpkgs { }
, ghc ? "default"
}:

import "${sources.nix-hs}/nix/shell.nix" {
  inherit pkgs;
  compiler = ghc;
}
