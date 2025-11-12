{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";

    utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, utils, ... }:
    let
      compiler = "ghc984";
      lix = "lix_2_93";

      overlay = final: prev: {
        lix = final.lixPackageSets.${lix}.lix;

        cabal2nix-unwrapped =
          final.haskell.lib.justStaticExecutables
            final.haskell.packages."${compiler}".cabal2nix;

        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            "${compiler}" = prev.haskell.packages."${compiler}".override (old: {
              overrides = final.lib.fold final.lib.composeExtensions (old.overrides or (_: _: { })) [
                (final.haskell.lib.packageSourceOverrides {
                  nix-serve-ng = ./.;
                  lix-serve-ng = ./.;
                })
                (haskellPackagesNew: haskellPackagesOld: {
                  nix-serve-ng = final.haskell.lib.overrideCabal haskellPackagesOld.nix-serve-ng (old: {
                    executableSystemDepends = (old.executableSystemDepends or [ ]) ++ [
                      final.boost.dev
                      final.nixVersions.nix_2_28
                    ];
                  });
                  lix-serve-ng = final.haskell.lib.overrideCabal haskellPackagesOld.lix-serve-ng (old: {
                    pname = "lix-serve-ng";
                    configureFlags = (old.configureFlags or [ ]) ++ [ "-flix" ];
                    executableSystemDepends = (old.executableSystemDepends or [ ]) ++ [
                      final.boost.dev
                      final.capnproto
                      final.lix
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

        lix-serve-ng =
          final.haskell.lib.justStaticExecutables
            final.haskell.packages."${compiler}".lix-serve-ng;
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

        inherit (pkgs) nix-serve-ng lix-serve-ng;

      in
      rec {
        packages = {
          inherit pkgs nix-serve-ng lix-serve-ng;
          default = nix-serve-ng;
        };

        defaultPackage = packages.default;

        apps = rec {
          default = nix-serve-ng;
          nix-serve-ng = {
            type = "app";
            program = "${nix-serve-ng}/bin/nix-serve";
          };
          lix-serve-ng = {
            type = "app";
            program = "${lix-serve-ng}/bin/nix-serve";
          };
        };

        defaultApp = apps.default;

        devShells = rec {
          default = nix-serve-ng;
          nix-serve-ng = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".nix-serve-ng).env;
          lix-serve-ng = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".lix-serve-ng).env;
        };

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
