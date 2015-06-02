let pkgs = (import <nixpkgs> {});
    haskellPackages = pkgs.haskell.packages.ghc784;
    myPkg = haskellPackages.callPackage (import ./default.nix) {};
in pkgs.myEnvFun {
    name = myPkg.name;
    buildInputs = 
       [(haskellPackages.ghcWithPackages (hs: ([
         hs.cabal-install
         hs.hscolour
       ] ++ myPkg.propagatedNativeBuildInputs)))];
     }
