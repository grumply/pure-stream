{ mkDerivation, base, pure-elm, pure-intersection, stdenv }:
mkDerivation {
  pname = "pure-stream";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm pure-intersection ];
  homepage = "github.com/grumply/pure-stream";
  description = "Streaming views";
  license = stdenv.lib.licenses.bsd3;
}
