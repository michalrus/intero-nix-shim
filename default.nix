{ nixpkgs ? import <nixpkgs> { }, haskellPackages ? nixpkgs.haskellPackages }:

with nixpkgs;

let

  build = let # TODO: Consider using `cabal sdist`? https://git.io/vSo8l
    src = builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
         lib.all (i: toString i !=            path) [ ./.git ./dist ./result ]
      && lib.all (i:          i != baseNameOf path) [ ".stack-work" ])
      ./.;
  in haskellPackages.callCabal2nix "intero-nix-shim" src {};

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = with haskellPackages; [ cabal-install hlint hindent stylish-haskell intero ];
  });

in build // { inherit env; }
