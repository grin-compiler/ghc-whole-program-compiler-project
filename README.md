# GHC whole program compiler project

The project consists of **GHC-WPC** and the corresponding **External STG IR** and **tooling**.


GHC-WPC is an extended GHC that exports the STG IR `(.o_stgbin)` for the compiled modules and linker metadata (`.ghc_stgapp`) at application link time.  


This repo uses GHC-WPC via stack, so no worries, stack will download it and do the setup automatically, but **only on Ubuntu 16.04 x64**.
If you use macOS or Windows you have to compile GHC-WPC manually. See the developer instructions below.  
However `external-stg` package compiles with vanilla GHC also.

## Readings
- [Introducing GHC whole program compiler (GHC-WPC)](https://www.patreon.com/posts/introducing-ghc-38173710)

## External STG tools (Ext-STG)
- `gen-exe` main compiler driver, it produces executable from `.ghc_stgapp` files.
- `gen-obj` compiles STG IR files `.o_stgbin` to object code `.o`. (gen-exe calls it)
- `ext-stg` CLI tool for external STG IR, it can pretty print `.o_stgbin` files.

## Sample applications for GHC-WPC

There is a set of prepared applications in the [ghc-wpc-sample-programs](https://github.com/grin-compiler/ghc-wpc-sample-programs) repository that you can compile easily to try out GHC-WPC.
No special preparation needed if you use x64 Ubuntu 16.04. It's only the regular stack based workflow.

## Usage (user)

The user of External STG is the one who does not alter the Ext-STG IR, instead just uses it via the `external-stg` package.
I.e. `external-stg-compiler` is such an example.

**Important:** GHC-WPC has precompiled Ubuntu 16.04 x64 binary release, so the install is straigtforward thanks to stack.

1. Clone this repository.
   ```
   git clone git@github.com:grin-compiler/ghc-whole-program-compiler-project.git
   ```
2. Install the external stg tooling with the following command:
   ```
   stack --stack-root `pwd`/.stack-root install
   ```
   *NOTE:* the stack root is set to the local folder to prevent spamming the global stack sandbox.  
3. Use `gen-exe` and `ext-stg` from terminal. *(it should be in PATH due to the stack install)*

## Why?
- to make it easy to develop new backends for GHC without extending Cabal with new targets
- to facilitate compiler/PL reasearch that need real world input programs
- to allow whole program analysis (new insights might be adopted to incremental compilers)  
- to escape from GHC codebase to the surface Haskell that allows to use any library
- to allow program observation with arbitrary precision
- to make it easy to focus on the compiler backend development without hacking GHC
- to allow other compilers to target GHC/STG and the feature rich RTS 

## Usage (GHC-WPC developer)

If you change the External STG IR, then GHC-WPC must be recompiled. If you just would like to use Ext-STG IR in you project then please follow the **(user) usage** instructions.

1. Clone this repository.
   ```
   git clone --recursive git@github.com:grin-compiler/ghc-whole-program-compiler-project.git
   ```
2. Compile GHC-WPC in `./ghc-wpc` folder with Hadrian. 
3. Set the path to the local GHC-WPC build in the corresponding part of `./stack.yaml`.
4. Install the external stg tooling with the following command:
   ```
   stack --stack-root `pwd`/.stack-root install
   ```
   *NOTE:* the stack root is set to the local folder to prevent spamming the global stack sandbox.  
5. Use `gen-exe` and `ext-stg` from terminal. *(it should be in PATH due to the stack install)*

## TODO
**Ext-STG IR**
- export IdInfo
- export Tickish
