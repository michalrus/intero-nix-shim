{ nixpkgs ?
    # Pinned for CI reproducibility.
    (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      rev = "799435b7cab97a39893a104999b3bc589e1172b1";
      sha256 = "1x61hpkagydrf05y0sa1ynmi8z3sm2377f4f6yiqlj9yvkg57jv3";
    }) {})
, haskellPackages ? nixpkgs.haskellPackages }:

with nixpkgs;

let

  build = let # TODO: Consider using `cabal sdist`? https://git.io/vSo8l
    src = builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
         lib.all (i: toString i !=            path) [ ./.git ./dist ./dist-newstyle ./result ]
      && lib.all (i:          i != baseNameOf path) [ ".stack-work" ])
      ./.;
  in lib.overrideDerivation (haskellPackages.callCabal2nix "intero-nix-shim" src {}) (oldAttrs: {
    postInstall = ''
      mkdir -p $out/libexec
      ln -s ${haskellPackages.cabal-install}/bin/cabal  $out/libexec
      ln -s ${haskellPackages.intero       }/bin/intero $out/libexec
      mv $out/bin/intero-nix-shim-exe $out/bin/intero-nix-shim
    '';
  });

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = [ git indent ] ++ (with haskellPackages; [
      hlint hindent stylish-haskell
      cabal-install intero # needed for ./dogfood.sh
    ]);
  });

in build // { inherit env; }
