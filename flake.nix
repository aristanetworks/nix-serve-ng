{
  inputs = {
    # Temporary, until Nixpkgs master has Nix 2.28
    nixpkgs.url = "github:NixOS/nixpkgs/master";

    utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, utils, ... }:
    let
      compiler = "ghc94";

      overlay = final: prev: {
        cabal2nix-unwrapped =
          final.haskell.lib.justStaticExecutables
            final.haskell.packages."${compiler}".cabal2nix;

        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            "${compiler}" = prev.haskell.packages."${compiler}".override (old: {
              overrides = final.lib.fold final.lib.composeExtensions (old.overrides or (_: _: { })) [
                (final.haskell.lib.packageSourceOverrides {
                  nix-serve-ng = ./.;

                  base16 = "1.0";
                })
                (haskellPackagesNew: haskellPackagesOld: {
                  nix-serve-ng = final.haskell.lib.overrideCabal haskellPackagesOld.nix-serve-ng (old: {
                    executableSystemDepends = (old.executableSystemDepends or [ ]) ++ [
                      final.boost.dev
                      final.nixVersions.nix_2_28
                    ];
                  });
                })
              ];
            });
          };
        };

        nix-serve-ng =
          final.haskell.lib.justStaticExecutables
            final.haskell.packages."${compiler}".nix-serve-ng;
      };

    in
    utils.lib.eachDefaultSystem (
      system:
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
          (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".nix-serve-ng).env;

        devShell = devShells.default;
      }
    )
    // rec {
      overlays = {
        # The default overlay only adds the exports for
        # `pkgs.haskell.packages."${compiler}".nix-serve-ng` and
        # `pkgs.nix-serve-ng`
        default = overlay;

        # This overlay additionally overrides `pkgs.nix-serve` to refer to
        # `pkgs.nix-serve-ng`
        override = final: prev: {
          nix-serve = final.nix-serve-ng;
        };
      };

      # The NixOS module is a thin wrapper around the overlay to replace
      # `nix-serve` with `nix-serve-ng`
      #
      # You can continue to use the old `services.nix-serve` NixOS options.
      nixosModules.default = {
        nixpkgs.overlays = [
          overlays.default
          overlays.override
        ];
      };
    };
}
