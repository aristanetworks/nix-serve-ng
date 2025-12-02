{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";

    lix-2_92_3.url = "git+https://git.lix.systems/lix-project/lix?ref=2.92.3";
    lix-2_93_3.url = "git+https://git.lix.systems/lix-project/lix?ref=2.93.3";
    lix-2_94_0.url = "git+https://git.lix.systems/lix-project/lix?ref=2.94.0";

    utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, utils, ... }@inputs:
    let
      compiler = "ghc984";

      patches = {
        lix = {
          "2.94.0" = [
            ./patches/lix/2.94.0/pkg-config.patch
            ./patches/lix/2.94.0/version-macros.patch
          ];
        };
      };

      # All variants known by the flake, more can be added via overlay.
      variants = {
        nix-serve-ng = null;
        lix-serve-ng = null;
        lix-serve-ng-2_92_3 = null;
        lix-serve-ng-2_93_3 = null;
        lix-serve-ng-2_94_0 = null;
      };

      forEachVariant = f: builtins.mapAttrs (name: _: f name) variants;

      overlay = final: prev: {
        nix-serve-ng-pkgs = {
          nix-versions = {
            nix-serve-ng = final.nixVersions.nix_2_28;
            lix-serve-ng = final.lix;
            lix-serve-ng-2_92_3 = inputs.lix-2_92_3.packages.${final.system}.nix;
            lix-serve-ng-2_93_3 = inputs.lix-2_93_3.packages.${final.system}.nix;
            lix-serve-ng-2_94_0 = inputs.lix-2_94_0.packages.${final.system}.nix;
          };

          packages = final.nix-serve-ng-pkgs.forEachNix (name: _:
            final.haskell.lib.justStaticExecutables final.haskell.packages.${compiler}.${name}
          );

          forEachNix = f: builtins.mapAttrs f final.nix-serve-ng-pkgs.nix-versions;
          forEachPackage = f: builtins.mapAttrs f final.nix-serve-ng-pkgs.packages;
        };

        cabal2nix-unwrapped =
          final.haskell.lib.justStaticExecutables
            final.haskell.packages."${compiler}".cabal2nix;

        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            "${compiler}" = prev.haskell.packages."${compiler}".override (old: {
              overrides = final.lib.fold final.lib.composeExtensions (old.overrides or (_: _: { })) [
                (final.haskell.lib.packageSourceOverrides {
                  nix-serve-ng = ./.;
                })
                (haskellPackagesNew: haskellPackagesOld:
                  let
                    mkPkg = pname: nix: final.haskell.lib.overrideCabal haskellPackagesOld.nix-serve-ng (old:
                      let
                        patchList = patches.${nix.pname}.${nix.version} or [ ];
                        requires-patched-nix = patchList != [ ];
                        patchedNix =
                          if requires-patched-nix
                          then nix.overrideAttrs (old: { patches = (old.patches or [ ]) ++ patchList; })
                          else nix;
                      in {
                        inherit pname;

                        configureFlags = (old.configureFlags or [ ])
                          ++ final.lib.optional (nix.pname == "lix") "-flix";

                        executableSystemDepends = (old.executableSystemDepends or [ ]) ++ [
                          patchedNix
                          final.boost.dev
                        ] ++ final.lib.optionals (nix.pname == "lix" && builtins.compareVersions nix.version "2.93.0" >= 0) [
                          final.capnproto
                        ];

                        passthru = (old.passthru or { }) // {
                          inherit requires-patched-nix;
                          nix = patchedNix;
                        };
                      }
                    );
                  in
                  final.nix-serve-ng-pkgs.forEachNix mkPkg
                )
              ];
            });
          };
        };
      }
        # Avoiding using final.nix-serve-ng-pkgs.forEachNix here to avoid infinite recursion.
        # This means variants added in user overlays will not be promoted.
        // forEachVariant (name: final.nix-serve-ng-pkgs.packages.${name});

    in
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          config = { };

          overlays = [ overlay ];

          inherit system;
        };

        # The variants in the flake are static, so this allows us to get the attrNames
        # without evaluated the overlay.
        fastPackages = forEachVariant (name: pkgs.nix-serve-ng-pkgs.packages.${name});
      in
      rec {
        packages = fastPackages // {
          inherit pkgs;
          default = packages.nix-serve-ng;
        };

        defaultPackage = packages.default;

        apps = forEachVariant (name: {
          type = "app";
          program = "${packages.${name}}/bin/nix-serve";
        })
          // { default = apps.nix-serve-ng; };

        defaultApp = apps.default;

        devShells = forEachVariant (name:
          (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".${name}).env
        )
          // { default = devShells.nix-serve-ng; };

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
