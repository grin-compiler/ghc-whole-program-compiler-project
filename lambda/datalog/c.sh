#!/bin/bash

set -x -e

souffle -c -j6 -o lambda-cfa main.dl
