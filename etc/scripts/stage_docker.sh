#!/usr/bin/env bash
set -ex
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
mkdir -p etc/docker/_artifacts
stack build
stack install --local-bin-path=etc/docker/_artifacts "$@"
bzip2 -zk etc/docker/_artifacts/*
mkdir -p etc/docker/_artifacts/bz2
mv etc/docker/_artifacts/*.bz2 etc/docker/_artifacts/bz2