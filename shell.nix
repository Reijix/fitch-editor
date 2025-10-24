{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  name = "hls";
  buildInputs = let
    hls = haskell-language-server.override { supportedGhcVersions = [ "9122" ]; };
    # Hack to get cabal-install 3.14 instead of 3.16 which is not compatible with hls yet.
    # https://lazamar.co.uk/nix-versions/
    cabal-pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
        sha256 = "sha256:0m0xmk8sjb5gv2pq7s8w7qxf7qggqsd3rxzv3xrqkhfimy2x7bnx";
    }) { inherit system; };

    my-cabal-install = cabal-pkgs.cabal-install;
  in [
    zlib.dev
    zlib
    my-cabal-install
    hls
    haskell.compiler.ghc9122
    haskellPackages.cabal-gild
    haskellPackages.ghcide
    haskellPackages.ghcid
    haskellPackages.hoogle
  ];

  shellHook = ''
    # ...
  '';
}

