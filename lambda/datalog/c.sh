#!/bin/bash

set -x -e

souffle -c -j`nproc --all` -o lambda-cfa main.dl
