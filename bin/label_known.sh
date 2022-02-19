#!/usr/bin/env sh
set -eux

stack run fc-crypto -- -h 940 -o ./output/labelKnown.svg labelKnown ./input/message.txt 7
