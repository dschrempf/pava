{
  description = "Greatest convex majorants and least concave minorants";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc921;
        pava = hpkgs.callCabal2nix "pava" self rec { };
        pava-dev = pkgs.haskell.lib.doBenchmark pava;
      in
      {
        packages.pava = pava;

        defaultPackage = pava;

        devShell = hpkgs.shellFor {
          packages = _: [ pava-dev ];
          buildInputs = with pkgs; [
            bashInteractive
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          doBenchmark = true;
          withHoogle = true;
        };
      }
    );
}
