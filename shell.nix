# this defines the Nix env to run this interactive Haskell project
with (import ./. { });
haskellPackages.shellFor {
  packages = p: with p; [ haze ];
  nativeBuildInputs = [ pkgs.cabal-install ];
  withHoogle = true;
}
