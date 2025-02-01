{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        oj-verify =
          with pkgs.python3Packages;
          pkgs.python3Packages.buildPythonApplication {
            name = "verification-helper";
            version = "5.6.0";
            pyproject = true;
            src = pkgs.fetchFromGitHub {
              owner = "online-judge-tools";
              repo = "verification-helper";
              rev = "adbff121b1f96de5f34e9f1483eb47d661c54075";
              fetchSubmodules = false;
              sha256 = "sha256-f7Ge8kLRQv9uxdNGtgNsypGVY0XAnKPCg8HYQ5nT6mI=";
            };
            build-system = [ setuptools ];
            dependencies = [
              colorlog
              importlab
              online-judge-tools
              pyyaml
              setuptools
              toml
            ];
            propagatedBuildInputs = [ setuptools ];
          };
      in
      {
        devShells.default =
          with pkgs;
          mkShell {
            buildInputs = [
              pkg-config
              stack
              cabal-install
              llvmPackages_15.bintools
            ];

            packages = [
              just
              oj-verify
              online-judge-tools
              python312Packages.selenium
              python312Packages.pyaml
              python312Packages.importlab

              haskell.compiler.ghc946.override { useLLVM = true; }
              (haskell-language-server.override { supportedGhcVersions = [ "946" ]; })
              # haskellPackages.doctest
              # TODO: fix doctest version
              haskell.packages.ghc946.cabal-fmt
              haskell.packages.ghc946.cabal-plan
              haskell.packages.ghc946.doctest
              haskell.packages.ghc946.implicit-hie

              hlint
              haskellPackages.hoogle
              haskellPackages.ghcid
              haskellPackages.ghcide
            ];
          };
      }
    );
}
