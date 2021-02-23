#!/usr/bin/env python3

import glob, os, os.path
import subprocess

test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/'

print('Scanning:', test_path)

test_list = list(sorted(glob.glob(test_path + "**/*.run", recursive=True)))

print('Found:', len(test_list), 'tests\n')

for test_path in test_list:
  print("delete", test_path)
  subprocess.run(['rm', '-fr', test_path])
