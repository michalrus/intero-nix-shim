[![Build Status](https://travis-ci.org/michalrus/intero-nix-shim.svg?branch=master)](https://travis-ci.org/michalrus/intero-nix-shim)

# intero-nix-shim

## High-level view

You need to have `default.nix` and `shell.nix` defined for your project, see https://nixos.org/nixpkgs/manual/#how-to-create-nix-builds-for-your-own-private-haskell-packages.

Emacs’ `intero-stack-executable` variable needs to point to `/nix/store/…-intero-nix-shim-…/bin/intero-nix-shim`.

You can set it to just `"intero-nix-shim"`, if it’s available in your global `PATH`. (By default, that variable is set to `"stack"`.)

## Simple how to

There are several ways to achieve the above.

Probably, the simplest one would be to install `haskellPackages.intero-nix-shim` globally in your `PATH` and set `intero-stack-executable` to just `"intero-nix-shim"` in `.dir-locals.el` for your project:

```elisp
;; your-project/.dir-locals.el

((nil . ((intero-stack-executable . "intero-nix-shim"))))
```

**Warning**: if you choose the `.dir-locals.el` way, Emacs has a security mechanism to prevent untrusted code execution. You will have to accept this setting. If you use `global-intero-mode`, Intero will start loading before you manage to whitelist this setting. Therefore, the first time, it will fail. Just do `M-x intero-restart`.

## Other options

You could add `intero-nix-shim` to your project’s `nix-shell` and have it compiled in the same environment as your project.

Then you’d have to somehow resolve its path in `.dir-locals.el`, e.g. by running `nix-shell --run 'command -v intero-nix-shell'`.

Or, simpler, by using https://github.com/shlevy/nix-buffer.

## Real world examples

For a self-contained one, please, clone https://github.com/michalrus/kornel, visit its `Main.hs` in Emacs, accept its `.dir-locals.el`, start `(intero-mode)`, and wait a moment. Consider running `nix-shell --pure --run 'intero-nix-shim --help'` in a terminal in the project’s directory first, not to freeze your Emacs for a significant time. Also, when opening new Haskell buffers in this project, the value for `intero-stack-executable` will need to be recalculated by Nix, significantly slowing you down. That’s why, performance wise, it’s best to install `intero-nix-shim` globally. Compare the `.dir-locals.el` proposed above with ones in this project.
