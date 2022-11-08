#!/usr/bin/env sh
set -eux

stack run list-alphabet -- -w 480 -o ./output/alphabet.svg 5
