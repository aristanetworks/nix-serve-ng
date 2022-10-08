{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/master;

    utils.url = github:numtide/flake-utils;

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { nixpkgs, utils, ... }:
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

    in
      utils.lib.eachDefaultSystem (system:
        let
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

              program = "${nix-serve-ng}/bin/nix-serve";
            };

            defaultApp = apps.default;

            devShells.default =
              (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.nix-serve-ng).env;

            devShell = devShells.default;
          }) // rec {
        overlays = {
          # The default overlay only adds the exports for
          # `pkgs.haskellPackages.nix-serve-ng` and `pkgs.nix-serve-ng`
          default = [ overlay ];

          # This overlay additionally overrides `pkgs.nix-serve` to refer to
          # `pkgs.nix-serve-ng`
          override =
            let
              replace = pkgsNew: pkgsOld: {
                nix-serve = pkgsNew.nix-serve-ng;
              };

            in
              [ overlay replace ];
        };

        # The NixOS module is a thin wrapper around the overlay to replace
        # `nix-serve` with `nix-serve-ng`
        #
        # You can continue to use the old `services.nix-serve` NixOS options.
        nixosModules.default = {
          nixpkgs.overlays = overlays.override;
        };
      };
}
