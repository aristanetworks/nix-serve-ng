let
  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        pkgsNew.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (haskellPackagesNew: haskellPackagesOld: {
            nix-serve-ng =
              pkgsNew.haskell.lib.overrideCabal
                (haskellPackagesNew.callPackage ./nix/nix-serve-ng.nix {
                  nixstore = pkgsNew.nix;
                  nixutil = pkgsNew.nix;
                })
                (old: {
                  executableSystemDepends = (old.executableSystemDepends or []) ++ [
                    pkgsNew.boost.dev
                  ];
                });
          });
    });
  };

  pkgs = import <nixpkgs> { config = { }; overlays = [ overlay ]; };

in
  pkgs.haskellPackages.nix-serve-ng.env
