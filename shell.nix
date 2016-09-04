with (import <nixpkgs> {}).pkgs;
let
    overrideAttrs = package: newAttrs: package.override (args: args // {
        mkDerivation = expr: args.mkDerivation (expr // newAttrs);
      });
    hp = pkgs.haskell.packages.ghc7103.override{
        overrides = self: super: {
          lens = overrideAttrs super.lens {doCheck = false;};
        };
      };
in (hp.callPackage ./. {}).env

