{ nixpkgs ? import <nixpkgs> { }, haskellPackages ? nixpkgs.haskellPackages }:

with nixpkgs;

let

  build = let # TODO: Consider using `cabal sdist`? https://git.io/vSo8l
    src = builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
         lib.all (i: toString i !=            path) [ ./.git ./dist ./result ]
      && lib.all (i:          i != baseNameOf path) [ ".stack-work" ])
      ./.;
  in lib.overrideDerivation (haskellPackages.callCabal2nix "intero-nix-shim" src {}) (oldAttrs: {
    postInstall = ''
      mkdir -p $out/libexec
      ln -s ${haskellPackages.cabal-install}/bin/cabal  $out/libexec
      ln -s ${haskellPackages.intero       }/bin/intero $out/libexec
    '';
  });

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = [ git indent ] ++ (with haskellPackages; [
      hlint hindent stylish-haskell
      cabal-install intero # needed for ./dogfood.sh
    ]);
  });

in build // { inherit env; }
