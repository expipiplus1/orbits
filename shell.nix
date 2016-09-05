with (import <nixpkgs> {}).pkgs;
let
    overrideAttrs = package: newAttrs: package.override (args: args // {
        mkDerivation = expr: args.mkDerivation (expr // newAttrs);
      });
    hp = pkgs.haskell.packages.ghc7103.override{
        overrides = self: super: {
          lens = overrideAttrs super.lens {doCheck = false;};
          exact-real = overrideAttrs super.exact-real {
            src = fetchFromGitHub{
              owner = "expipiplus1";
              repo = "exact-real";
              rev = "16bad7143ea0af9202c6dd97eb96d13948a489a5";
              sha256 = "1i6nc9y64xp4iq3zcqmn0h4vrwlra3392jhjvdy27ydllld05jal";
            };
          };
          memoize = overrideAttrs super.memoize {
            editedCabalFile = null;
            revision = null;
            src = fetchFromGitHub{
              owner = "jwaldmann";
              repo = "memoize";
              rev = "a6f0d6ea4ea478db17d0f623ab686837d52a5a42";
              sha256 = "0b9z2hqpgckmwbmsysv42hanaj6653v1wyj247gpzwp653jw5jhp";
            };
          };
          uom-plugin = overrideAttrs super.uom-plugin {
            src = fetchFromGitHub{
              owner = "adamgundry";
              repo = "uom-plugin";
              rev = "0ea56180b0adecb83ee5dbcb9e46a640b72d0acc";
              sha256 = "14pm5wlqsaf73ig9mrn7gdrxj32b22181n5g06r64ah78ir0bdgb";
            } + "/uom-plugin";
          };
        };
      };
in (hp.callPackage ./. {}).env

