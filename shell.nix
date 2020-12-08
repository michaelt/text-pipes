let sources = import nix/sources.nix;
in
{ pkgs ? import sources.nixpkgs { }
, ghc ? "default"
}:
let
  nix-hs = import sources.nix-hs {
    inherit pkgs;
  };

  drv = nix-hs {
    cabal = ./pipes-text.cabal;
    compiler = ghc;
  };

  maintainerScripts =
    import sources.haskellrc { inherit pkgs; };

in
drv.interactive.overrideAttrs (orig: {
  buildInputs = (orig.buildInputs or [ ]) ++ [
    maintainerScripts
  ];
})
