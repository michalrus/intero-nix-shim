[![Build Status](https://travis-ci.org/michalrus/intero-nix-shim.svg?branch=master)](https://travis-ci.org/michalrus/intero-nix-shim)

# intero-nix-shim

## High-level view

You need to have `default.nix` and `shell.nix` defined for your project, see https://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages.

Emacs’ `intero-stack-executable` variable needs to point to `/nix/store/…-intero-nix-shim-…/bin/intero-nix-shim`.

You can set it to just `"intero-nix-shim"`, if it’s available in your global `PATH`. (By default, that variable is set to `"stack"`.)

## Simple how to

There are several ways to achieve the above.

Probably, the simplest one would be to install `intero-nix-shim` globally in your `PATH`, e.g.:

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
