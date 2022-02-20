#!/usr/bin/env sh
set -eux

stack run render-message -- -h 940 -o ./output/original_message.svg ./input/message.txt 7
