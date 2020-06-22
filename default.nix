{ mkDerivation, base, pure-core, pure-intersection, pure-default, pure-elm, pure-prop, stdenv }:
mkDerivation {
  pname = "pure-stream";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-intersection pure-default pure-elm pure-prop ];
  homepage = "github.com/grumply/pure-stream";
  description = "Streaming views";
  license = stdenv.lib.licenses.bsd3;
}
