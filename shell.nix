with (import <nixpkgs> {}).pkgs;
(pkgs.haskellPackages.callPackage ./. {}).env
