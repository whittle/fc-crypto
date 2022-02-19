#!/usr/bin/env sh
set -eux

stack run fc-crypto -- -h 940 -o ./output/original.svg original ./input/message.txt 7
