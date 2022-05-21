#!/bin/bash

set -x -e

souffle -c -j`nproc --all` -o called-by-outer called-by-outer.dl
