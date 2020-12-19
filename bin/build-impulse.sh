#!/usr/bin/env bash
set -xe
PRJ="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"/..
cp -f -v \
  $( cd $PRJ/impulse && nix-build --no-out-link)/impulse.js \
  $PRJ/neuron/ghcjs/impulse.js
