{ mkDerivation, base, base16, base32, bsd-sysctl, bytestring
, charset, http-types, lib, managed, megaparsec, mtl, network
, nixstore, nixutil, optparse-applicative, vector, wai, wai-extra
, warp, warp-tls
}:
mkDerivation {
  pname = "nix-serve-ng";
  version = "1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base16 base32 bsd-sysctl bytestring charset http-types managed
    megaparsec mtl network optparse-applicative vector wai wai-extra
    warp warp-tls
  ];
  executableSystemDepends = [ nixstore nixutil ];
  description = "A drop-in replacement for nix-serve that's faster and more stable";
  license = lib.licenses.asl20;
}
