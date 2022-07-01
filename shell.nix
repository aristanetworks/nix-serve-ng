let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/87d9c84817d7be81850c07e8f6a362b1dfc30feb.tar.gz";
    sha256 = "0pcasnmdbs2lrsp4m8ww06xd7b318agfygi61qjsizb328nxhcqa";
  };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        pkgsNew.lib.composeExtensions
          (old.overrides or (_: _: { }))
          (haskellPackagesNew: haskellPackagesOld: {
            nix-serve-ng =
              pkgsNew.haskell.lib.overrideCabal
                (haskellPackagesNew.callPackage ./nix/nix-serve-ng.nix { })
                (old: {
                  executableSystemDepends = (old.executableSystemDepends or []) ++ [
                    pkgsNew.boost.dev
                  ];
                });
          });
    });
  };

  pkgs = import nixpkgs { config = { }; overlays = [ overlay ]; };

in
  pkgs.haskellPackages.nix-serve-ng.env
