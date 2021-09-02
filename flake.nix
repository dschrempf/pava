{
  description = "Greatest convex majorants and least concave minorants";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          haskellPackages = pkgs.haskellPackages;
          pava = haskellPackages.callCabal2nix "pava" self rec {};
          pava-dev = pkgs.haskell.lib.doBenchmark pava;
        in
          {
            packages.pava = pava;

            defaultPackage = pava;

            devShell = pkgs.haskellPackages.shellFor {
              packages = _: [ pava-dev ];
              buildInputs = with pkgs; [
                haskellPackages.cabal-install
                haskellPackages.haskell-language-server
                haskellPackages.stack
              ];
              doBenchmark = true;
            };
          }
    );
}
