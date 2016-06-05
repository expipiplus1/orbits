/* Build instructions for the continuous integration system Hydra. */

{ releaseBuild ? false
, supportedSystems ? ["x86_64-linux"] ++ (if releaseBuild then ["i686-linux" "x86_64-darwin"] else [])
, ghcVer ? "ghc7103"
}:

with import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; };

let 

  overrides = self: super: {
    orbit = self.callPackage ./default.nix {};
  };

  mkJob = attr: testOn supportedSystems (pkgs: ((pkgs.haskell.packages.${ghcVer}.override { inherit overrides; })).${attr});

in 

  mkJob "orbit"
