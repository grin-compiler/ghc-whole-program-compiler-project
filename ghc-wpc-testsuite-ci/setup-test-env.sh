reset
set -e -x

# TODO: abstarct away ghc version
# TODO: abstarct away the test directory we'd like to run


# stage 1: setup and run ghc tests unmodified
# download ghc testsuite
curl -L https://downloads.haskell.org/~ghc/9.2.7/ghc-9.2.7-testsuite.tar.xz -o ghc-9.2.7-testsuite.tar.xz
tar xf ghc-9.2.7-testsuite.tar.xz

# boot library tests are included in ghc source
curl -L https://downloads.haskell.org/~ghc/9.2.7/ghc-9.2.7-src.tar.xz -o ghc-9.2.7-src.tar.xz
tar xf ghc-9.2.7-src.tar.xz

# patch to save the detailed test output
patch -d ./ghc-9.2.7 -s -p1 < ghc-9.2.7-testsuite.patch

stack build
cd ghc-9.2.7/testsuite/TEST_DIR_TO_RUN

# ghc testsuite make fails at the first run
set +e
stack exec make -- boot
set -e

# LIBRARIES_TO_TEST control which package's tests to run ; "" means all available
stack exec make -- CLEANUP=0 THREADS=`nproc` RUNNABLE_ONLY=1 PACKAGE_DB=`stack path --snapshot-pkg-db` LIBRARIES_TO_TEST=""

# stage 2: run estgi test driver ; it will pick up previously compiled tests

# TODO: compile estgi test driver
run-stgi-testsuite *
