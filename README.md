[![Build Status](https://travis-ci.org/michalrus/intero-nix-shim.svg?branch=master)](https://travis-ci.org/michalrus/intero-nix-shim)

# intero-nix-shim

----

## WARNING

**This project will probably never work with multiple Cabal targets per project — too much effort to emulate this behavior of the original Stack. See issues #7, #6.**

We suggest using Dante, which works very well with Cabal@Nix (incl. multiple targets), or HIE, after https://github.com/haskell/haskell-ide-engine/issues/357 is ready.

----

## High-level view

You need to have `default.nix` and `shell.nix` defined for your project, see https://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages.

Emacs’ `intero-stack-executable` variable needs to point to `/nix/store/…-intero-nix-shim-…/bin/intero-nix-shim`.

You can set it to just `"intero-nix-shim"`, if it’s available in your global `PATH`. (By default, that variable is set to `"stack"`.)

## Simple how to

There are several ways to achieve the above.

Probably, the simplest one would be to install `intero-nix-shim` globally in your `PATH`, e.g., if you’re on `nixos-17.03`:

```nix
# /etc/nixos/configuration.nix

{
  environment.systemPackages = [
    haskellPackages.intero-nix-shim
  ];
}
```

… and set `intero-stack-executable` to just `"intero-nix-shim"` in `.dir-locals.el` for your project:

```elisp
;; your-project/.dir-locals.el

((nil . ((intero-stack-executable . "intero-nix-shim"))))
```

**Warning**: if you choose the `.dir-locals.el` way, Emacs has a security mechanism to prevent untrusted code execution. You will have to accept this setting. If you use `global-intero-mode`, Intero will start loading before you manage to whitelist this setting. Therefore, the first time, it will fail. Just do `M-x intero-restart`.
