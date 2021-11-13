# GHC whole program compiler project

The project consists of **GHC-WPC** and the corresponding **External STG IR** and **tooling**.


GHC-WPC is an extended GHC that exports the STG IR `(.modpak)` for the compiled modules and linker metadata (`.ghc_stgapp`) at application link time.  

<img height="350" src="https://user-images.githubusercontent.com/877489/114280753-0d311300-9a3b-11eb-8d50-facad35f0e9a.png"/>

The `external-stg-compiler` package should be compiled with GHC-WPC but the other packages i.e. `external-stg` compiles with vanilla GHC also.

## Readings
- [Introducing GHC whole program compiler (GHC-WPC)](https://www.patreon.com/posts/introducing-ghc-38173710)
- [GHC-WPC thread on ghc-devs mailing list](https://mail.haskell.org/pipermail/ghc-devs/2020-June/018994.html)
- [External STG Interpreter](https://www.patreon.com/posts/external-stg-49857800)

## External STG tools (Ext-STG)
- `gen-exe` main compiler driver, it produces executable from `.ghc_stgapp` files.
- `gen-obj` compiles STG IR files `.o_stgbin` to object code `.o`. (gen-exe calls it)
- `ext-stg` CLI tool for external STG IR, it can pretty print `.o_stgbin` files.

## Sample applications for GHC-WPC

There is a set of prepared applications in the [ghc-wpc-sample-programs](https://github.com/grin-compiler/ghc-wpc-sample-programs) repository that you can compile easily to try out GHC-WPC.
<!--
No special preparation needed if you use x64 Debian9, Ubuntu 16.04-17.10. It's only the regular stack based workflow.
## Usage (user)
### DOES NOT WORK AT THE MOMENT, NEW BINARY RELEASE OF GHC-WPC IS NEEDED
### try the GHC-WPC developer usage way

The user of External STG is the one who does not alter the Ext-STG IR, instead just uses it via the `external-stg` package.
I.e. `external-stg-compiler` is such an example.

**Important:** GHC-WPC has precompiled x64 Debian9, Ubuntu 16.04-17.10 binary release, so the install is straigtforward thanks to stack.

1. Clone this repository.
   ```
   git clone git@github.com:grin-compiler/ghc-whole-program-compiler-project.git
   ```
2. Install the external stg tooling with the following commands:
   ```
   (cd mod-pak ; stack install)
   stack --stack-root `pwd`/.stack-root install
   ```
   *NOTE:* the stack root is set to the local folder to prevent spamming the global stack sandbox.  
3. Use `gen-exe` and `ext-stg` from terminal. *(it should be in PATH due to the stack install)*
-->
## Why?
- to make it easy to develop new backends for GHC without extending Cabal with new targets
- to facilitate compiler/PL research that needs real world programs to analyse
- to allow whole program analysis (new insights might be adopted to incremental compilers)  
- to escape from GHC codebase to the mainstream Haskell UX/DX that allows to use any library
- to allow program observation with arbitrary precision
- to make it easy to focus on the compiler backend development without hacking GHC
- to allow other compilers to target GHC/STG and the feature rich RTS 

## Usage

If you change the External STG IR, then GHC-WPC must be recompiled.

0. Install (exact version):
   - GHC 8.8.3
   - happy 1.19.12
   - alex 3.2.5
1. Clone this repository.
   ```
   git clone --recursive git@github.com:grin-compiler/ghc-whole-program-compiler-project.git
   ```
2. Install modpak tooling
   ```
   (cd mod-pak ; stack install)
   ```
3. Compile GHC-WPC in `./ghc-wpc` folder with Hadrian (see [ghc.dev](https://ghc.dev) for details).
   ```
   ./boot
   ./configure
   hadrian/build-stack -j
   ```
   **IMPORTANT:** use hadrian/build-stack

4. **At this point you have a working GHC-WPC.**  
   The next steps are about the compilation of GHC-WPC tooling and the usage of GHC-WPC.

5. Set the path to the local GHC-WPC build in the corresponding part of `./stack.yaml`.
   change the following line to your GHC-WPC build path:
   ```
   extra-path:
     - /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin
   ```
   i.e. set the `USER` and `PROJECT` part properly (`./stack.yaml` line 32) 
   ```
   extra-path:
     - /home/USER/PROJECT/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin
   ```
6. Install the external stg tooling with the following commands:
   ```
   stack --stack-root `pwd`/.stack-root install
   ```
   *NOTE:* the stack root is set to the local folder to prevent spamming the global stack sandbox.  
7. Use `gen-exe` and `ext-stg` from terminal. *(it should be in PATH due to the stack install)*

## TODO
**Ext-STG IR**
- export IdInfo (without it `gen-exe` compiles -O0 executables)

## UnZip with Zstd support
The `.modpak` and `.fullpak` files use Zstd compression method that was introduced in the Zip 6.3.8 standard in 2020.  
The GHC-WPC tooling can handle Zstd zip files out of the box.  
But if you'd like to unpack the `.modpak` and `.fullpak` files manually then you'll need an `unzip` version with Zstd support.  
https://github.com/csabahruska/unzip-zstd
