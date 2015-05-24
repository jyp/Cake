{ mkDerivation, array, base, binary, bytestring, cmdargs
, containers, derive, directory, filepath, mtl, parsek, process
, pureMD5, regex-tdfa, split, stdenv
}:
mkDerivation {
  pname = "cake";
  version = "1.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    array base binary bytestring cmdargs containers derive directory
    filepath mtl parsek process pureMD5 regex-tdfa split
  ];
  description = "A build-system library and driver";
  license = "GPL";
}
