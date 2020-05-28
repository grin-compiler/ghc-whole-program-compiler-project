# GHC whole program compiler project

This repo uses a custom GHC (ghc-wpc). Stack will download it and set up automatically.

To install external stg tooling run the following command:
```
stack --stack-root `pwd`/.stack-root install
```
External STG tools:
- `gen-exe` main compiler driver, it produces executable from *.ghc_stgapp files.
- `gen-obj` compiles STG IR files *.o_stgbin to object code *.o. (gen-exe calls it)
- `ext-stg` CLI tool for external STG IR, it can pretty print *.o_stgbin files.
