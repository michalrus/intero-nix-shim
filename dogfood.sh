#!/bin/sh

set -o errexit -o nounset -o noclobber
CDPATH='' cd -- "$(dirname -- "$0")"
shellQuoteArguments(){ getopt --options "" --shell sh -- -- "$@" | cut -c 4- ; }
# Don’t add --pure here, or we won’t have `nix-shell` inside of `nix-shell`.
exec nix-shell --quiet --run "
  mkdir -p dist/build/libexec
  ln -sf \"\$(command -v cabal )\" dist/build/libexec/
  ln -sf \"\$(command -v intero)\" dist/build/libexec/
  exec cabal run --verbose=0 -- $(shellQuoteArguments "$@")
"
