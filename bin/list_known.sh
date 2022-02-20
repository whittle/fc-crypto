#!/usr/bin/env sh
set -eux

stack run list-known -- -w 480 -o ./output/known_glyphs.svg 4
