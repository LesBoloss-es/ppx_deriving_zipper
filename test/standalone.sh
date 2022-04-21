#!/bin/sh

set -euC

dune exec test/standalone.exe -- "$1" 2>&1 \
  | ocamlformat --impl --enable-outside-detected-project - \
  | {
      if command -v bat > /dev/null; then
        bat --language=ocaml --pager=never
      else
        echo "Install bat? ;)" >&2
        cat
      fi
    }
