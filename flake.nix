{

  inputs = {
    miso.url = "github:dmjio/miso";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {miso, self, nixpkgs, flake-utils }:
      flake-utils.lib.eachDefaultSystem (system: {
      devShells.default = let 
        pkgs = nixpkgs.legacyPackages.${system}; 
      in import ./shell.nix { inherit pkgs system; };
      devShells.wasm = miso.outputs.devShells.${system}.wasm.overrideAttrs {
        name = "wasm";
      };
      devShells.ghcjs = miso.outputs.devShells.${system}.ghcjs.overrideAttrs {
        name = "ghcjs";
      };
    });
}

