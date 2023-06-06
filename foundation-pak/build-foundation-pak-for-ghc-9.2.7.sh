set -x -e

############
# setup
############

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)
      machine=Linux
      sudo apt install python3-sphinx
      ;;

    Darwin*)
      machine=Mac

      brew install basictex
      export PATH="/usr/local/texlive/2023basic/bin/universal-darwin:$PATH"
      sudo tlmgr update --self
      sudo tlmgr install texliveonfly
      sudo tlmgr install tex-gyre
      sudo tlmgr install fncychap
      sudo tlmgr install adjustbox
      sudo tlmgr install tcolorbox
      sudo tlmgr install collectbox
      sudo tlmgr install ucs
      sudo tlmgr install environ
      sudo tlmgr install trimspaces
      sudo tlmgr install titling
      sudo tlmgr install enumitem
      sudo tlmgr install rsfs

      brew install python@3.11
      pip3 install sphinx==4.3.2
      ;;

    CYGWIN*)
      machine=Cygwin
      ;;

    MINGW*)
      machine=MinGw
      ;;

    MSYS_NT*)
      machine=Git
      ;;

    *)
      machine="UNKNOWN:${unameOut}"
esac

stack --resolver lts-20.24 install alex-3.2.6 happy-1.20.0 zip-cmd-1.0.1

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

ls -lah `pwd`/_build/foundation-pak/*.zip
ls -lah `pwd`/_build/bindist/*.xz
