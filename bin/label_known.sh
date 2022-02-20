#!/usr/bin/env sh
set -eux

stack run fc-crypto -- -h 940 -o ./output/label_known.svg ./input/message.txt 7
