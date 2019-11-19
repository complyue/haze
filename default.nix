{ overlays ? [ ], ... }@args:
import (
# to use a version of Hadui from github
  # builtins.fetchTarball {
  #   url = "https://github.com/complyue/hadui/archive/0.1.0.rc3.tar.gz";
  #   sha256 = "1pa13r33smzni1r263sbz3q2iay4qyavmcxpaslafzc1rh548hbj";
  # }

  # to use the version of Hadui checked out locally
  ../hadui
) (args // {
  overlays = [
    (self: super:
      let
        pkgsWithHaze = super.haskellPackages.override {
          overrides = hself: hsuper: {
            haze = hself.callCabal2nix "haze" ./haze { };
          };
        };
      in {
        # override the Haskell package set at standard locations
        haskellPackages = pkgsWithHaze;
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghcWithHaze = pkgsWithHaze;
          };
        };
      })
  ] ++ (args.overlays or [ ]);
})
