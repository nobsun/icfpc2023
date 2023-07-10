#! /bin/sh

set -x
scripts/gen-post-commands.sh > postsubs

xargs -P 15 -n 1 --delimiter='\n' ./scripts/run1.sh < postsubs
