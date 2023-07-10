#! /bin/sh

set -x
scripts/gen-submit-commands.sh > scommands.sh

xargs -P 15 -n 1 --delimiter='\n' ./scripts/run1.sh < scommands.sh
