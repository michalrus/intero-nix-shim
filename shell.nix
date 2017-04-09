{ nixpkgs ? import <nixpkgs> { }, haskellPackages ? nixpkgs.haskellPackages }: (import ./default.nix { inherit nixpkgs haskellPackages; }).env
