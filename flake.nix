{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "flake-utils";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
  };

  outputs = { self, flake-utils, nixpkgs, stacklock2nix }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];
      compilerVersion = "966";
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ stacklock2nix.overlay ];
        };
        appendGhcFlags = opts: drv:
          pkgs.haskell.lib.appendBuildFlag drv
          ''--ghc-options="${builtins.concatStringsSep " " opts}"'';
        insaneOptimizationFlags =
          [ "-O2" "-fexpose-all-unfoldings" "-fspecialise-aggressively" ];
        justStatic = package:
          (pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.justStaticExecutables package)
            (drv: { disallowGhcReference = false; }));
      in {
        packages = {
          default = justStatic (pkgs.stacklock2nix {
            stackYaml = ./stack.yaml;
            baseHaskellPkgSet = pkgs.haskell.packages."ghc${compilerVersion}";
            additionalHaskellPkgSetOverrides = (hfinal: hprev:
              builtins.mapAttrs (name: value:
                if (builtins.tryEval (!(builtins.isAttrs value)
                  || !(value ? pname))).value or true then
                  value
                else if value ? doCheck && value.pname != "ghc" then
                  pkgs.lib.pipe value (with pkgs.haskell.lib; [
                    dontCheck
                    dontHaddock
                    doJailbreak
                    doStrip
                    disableLibraryProfiling
                    disableExecutableProfiling
                    (appendGhcFlags insaneOptimizationFlags)
                  ])
                else
                  value) hprev);
          }).pkgSet.project-m36-rest;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler."ghc${compilerVersion}"
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [ compilerVersion ];
            })
          ];
        };
      });
  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];
    extra-trusted-public-keys =
      [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
  };
}
