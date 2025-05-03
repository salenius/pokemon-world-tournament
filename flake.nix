{
  description = "Pokemon World Tournament battle simulator in Haskell with PokeAPI data";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-new.url = "github:nixos/nixpkgs/nixos-unstable"; # version 25.05 ->
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-new, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs-new = import nixpkgs-new { inherit system; };
      in
        {
          devShell = pkgs.mkShell {
            # buildInputs is for dependencies you'd need "at run time",
            # were you to to use nix-build not nix-shell and build whatever you were working on
            buildInputs = with pkgs; [
              ghcid
              cabal-install
              ghc
              haskellPackages.haskell-language-server
              haskellPackages.recursion-schemes
              haskellPackages.doctest
	            haskellPackages.QuickCheck
              haskellPackages.aeson

              # Try out different CSV tools and which one is the best
              pkgs-new.xan # CSV parser
              jq # CSV -> JSON -> further manipulation

            ];
          };


        }
    );
}
