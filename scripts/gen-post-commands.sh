#! /bin/sh

ls -1 postapply/*_???.json | runghc -isrc scripts/submit-commands.hs
