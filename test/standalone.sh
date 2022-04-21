#!/bin/sh
set -euC

if out=$(dune exec test/standalone.exe -- "$1" 2>&1); then
  echo "$out" \
    | ocamlformat --impl --enable-outside-detected-project - \
    | {
        if command -v bat > /dev/null; then
          bat --language=ocaml --pager=never
        else
          echo "Install bat? ;)" >&2
          cat
        fi
      }
else
  printf 'Failed with return code %d. Output is:\n\n%s\n' $? "$out"
fi
