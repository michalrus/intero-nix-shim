# intero-nix-shim

## High-level view

1. `nix-shell --pure` started inside of you project’s directory should have the `intero` and `cabal` executables in its PATH. Check whether it works with:
    ```
    $ cd your-project/
    $ nix-shell --pure --run intero
    ```
1. `intero-stack-executable` Emacs variable needs to point to `/nix/store/…-intero-nix-shim-…/bin/intero-nix-shim`.

## Simple how to

There are several ways to achieve the above.

Probably, the simplest one would be to install `haskellPackages.intero-nix-shim` globally in your PATH and set `intero-stack-executable` to just `"intero-nix-shim"` in `.dir-locals.el` for your project:

```elisp
;; your-project/.dir-locals.el

((nil . ((intero-stack-executable . "intero-nix-shim"))))
```

Don’t forget to add `intero` and `cabal` to your project’s `nix-shell`, e.g. like that:

```nix
# your-project/default.nix

let

  compiler = haskell.packages.ghc802;

  build = compiler.callCabal2nix pname ./. {};

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = with compiler; [ cabal-install intero ];
  });

in build // { inherit env; }
```

```nix
# your-project/shell.nix

(import ./default.nix).env
```

## Other options

You could add `intero-nix-shim` to your project’s `nix-shell` and have it compiled in the same environment as your project.

Then you’d have to somehow resolve its path in `.dir-locals.el`, e.g. by running `nix-shell --run 'command -v intero-nix-shell'`.

Or, simpler, by using https://github.com/shlevy/nix-buffer.

## Examples

For a self-contained example, please, clone https://github.com/michalrus/kornel, visit its `Main.hs` in Emacs, accept its `.dir-locals.el`, start `(intero-mode)`, and wait a moment.
