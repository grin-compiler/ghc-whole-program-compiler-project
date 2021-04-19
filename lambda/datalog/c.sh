#!/bin/bash

set -x -e

souffle --legacy -c -j6 -o lambda-cfa main.dl
