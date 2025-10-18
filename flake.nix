{
  description = "A basic flake with a shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-for-ghc.url = "github:NixOS/nixpkgs/ebe4301cbd8f81c4f8d3244b3632338bbeb6d49c";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      nixpkgs,
      nixpkgs-for-ghc,
      flake-utils,
      treefmt-nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        ghcpkgs = import nixpkgs-for-ghc {
          inherit system;
        };
        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs = {
            # cabal-fmt.enable = true;
            cabal-gild.enable = true;
            ormolu.enable = true;
          };
        };
      in
      {
        devShells.default =
          with pkgs;
          mkShell {
            buildInputs = [
              pkg-config
              # stack
              cabal-install
              llvmPackages.bintools
            ];

            packages = [
              # verify
              online-judge-tools
              online-judge-verify-helper

              # GHC 9.8.4
              (ghcpkgs.haskell.compiler.ghc984.override { useLLVM = true; })
              (ghcpkgs.haskell-language-server.override { supportedGhcVersions = [ "984" ]; })
              ghcpkgs.haskell.packages.ghc984.cabal-gild
              # ghcpkgs.haskell.packages.ghc984.cabal-fmt
              ghcpkgs.haskell.packages.ghc984.cabal-plan
              ghcpkgs.haskell.packages.ghc984.doctest
              ghcpkgs.haskell.packages.ghc984.implicit-hie
              ghcpkgs.haskell.packages.ghc984.ormolu

              hlint
              ghcpkgs.haskellPackages.hoogle
              ghcpkgs.haskellPackages.ghcid
              ghcpkgs.haskellPackages.ghcide
              ghcpkgs.haskellPackages.ghci-dap
              ghcpkgs.haskellPackages.haskell-dap

              # CI
              act
              action-validator
              actionlint
            ];
          };

        # nix fmt
        formatter = treefmtEval.config.build.wrapper;

        # nix run .#treefmt
        packages.treefmt = treefmtEval.config.build.wrapper;

        # FIXME:
        # # nix flake check
        # checks.treefmt = treefmtEval.config.build.check;
      }
    );
}
