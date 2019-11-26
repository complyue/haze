{ overlays ? [ ], ... }@args:
import (
# to use a version of Hadui from github
#   builtins.fetchTarball {
#     url = "https://github.com/complyue/hadui/archive/0.1.0.2.tar.gz";
#     sha256 = "0lz2i7ycaj7pj82bjjkm5cchq3ds23v9i2pkc3hzq16p7hvb67lz";
#   }

  # to use the version of Hadui checked out locally
  ../hadui
) (args // {
  overlays = (args.overlays or [ ]) ++ [
    (self: super:
      let
        pkgsWithHaze = super.haskellPackages.override {
          overrides = hself: hsuper: {
            haze = hself.callCabal2nix "haze" ./haze { };
          };
        };
      in {
        # haze tobe a top-level package
        haze = pkgsWithHaze.haze;
        # override the Haskell package set at standard locations
        haskellPackages = pkgsWithHaze;
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghcWithHaze = pkgsWithHaze;
          };
        };
      })
  ];
})
