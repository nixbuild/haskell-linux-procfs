{ mkDerivation, attoparsec, base, binary, lib, text, time, unix }:
mkDerivation {
  pname = "linux-procfs";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ attoparsec base binary text time unix ];
  homepage = "https://github.com/nixbuild/haskell-linux-procfs";
  license = lib.licenses.mit;
}
