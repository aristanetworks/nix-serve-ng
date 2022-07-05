{ mkDerivation, async, base, base16, base32, bytestring, charset
, http-client, http-types, managed, megaparsec, mtl, network, nix
, optparse-applicative, stdenv, tasty-bench, temporary, turtle
, vector, wai, wai-extra, warp, warp-tls
}:
mkDerivation {
  pname = "nix-serve-ng";
  version = "1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base16 base32 bytestring charset http-types managed megaparsec
    mtl network optparse-applicative vector wai wai-extra warp warp-tls
  ];
  executablePkgconfigDepends = [ nix ];
  benchmarkHaskellDepends = [
    async base bytestring http-client tasty-bench temporary turtle
    vector
  ];
  description = "A drop-in replacement for nix-serve that's faster and more stable";
  license = stdenv.lib.licenses.asl20;
}
