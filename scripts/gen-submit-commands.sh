#! /bin/sh

ls -1 solutions/*_???.json | runghc -isrc scripts/submit-commands.hs
