#!/bin/bash

set -e

docker build \
       --file benchmark/Dockerfile \
       -t l2m-benchmark \
       .
