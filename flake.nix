{
  description = "A Haskell project managed with Stack and Nix Flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell tools
            stack
            haskell.compiler.ghc96
            haskell-language-server
            cabal-install

            # System dependencies
            zlib
          ];

          shellHook = ''
            echo "Entering Haskell development environment"
          '';
        };
      });
}
