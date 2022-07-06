{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/22.05;

    utils.url = github:numtide/flake-utils;

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        overlay = pkgsNew: pkgsOld: {
          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides =
              pkgsNew.lib.fold pkgsNew.lib.composeExtensions (old.overrides or (_: _: { })) [
                (pkgsNew.haskell.lib.packageSourceOverrides {
                  nix-serve-ng = ./.;
                })
                (haskellPackagesNew: haskellPackagesOld: {
                  nix-serve-ng =
                    pkgsNew.haskell.lib.overrideCabal
                      haskellPackagesOld.nix-serve-ng
                      (old: {
                        executableSystemDepends = (old.executableSystemDepends or []) ++ [
                          pkgsNew.boost.dev
                        ];
                      });
                })
              ];
          });

          nix-serve-ng =
            pkgsNew.haskell.lib.justStaticExecutables
              pkgsNew.haskellPackages.nix-serve-ng;
        };

        pkgs = import nixpkgs {
          config = { };

          overlays = [ overlay ];

          inherit system;
        };

        inherit (pkgs) nix-serve-ng;

      in
        rec {
          packages.default = nix-serve-ng;

          defaultPackage = packages.default;

          apps.default = {
            type = "app";

            program = "${nix-serve-ng}/bin/nix-serve-ng";
          };

          defaultApp = apps.default;

          devShells.default =
            (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.nix-serve-ng).env;

          devShell = devShells.default;
        });
}
