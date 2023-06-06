set -x -e

############
# setup
############

cabal update
cabal install --overwrite-policy=always alex-3.2.6 happy-1.20.0 zip-cmd-1.0.1

############
# checkout GHC source and build
############

mkdir foundation-pak-ghc-9.2.7-wpc
cd foundation-pak-ghc-9.2.7-wpc

git clone https://github.com/grin-compiler/ghc-wpc.git

cd ghc-wpc

git checkout ghc-9.2-wpc-design2

git submodule update --init --recursive

./boot
./configure

./hadrian/build-stack foundation-pak -j

./hadrian/build-stack binary-dist-xz -j

############
# output foundation pak and ghc-9.2.7-wpc bindist
############

ls -lah _build/foundation-pak/*.zip
ls -lah _build/bindist/*.xz
