# GHC whole program compiler project

The project consists of GHC **wpc-plugin** and the corresponding **External STG IR** and **tooling**.


The wpc-plugin is a compiler plugin for GHC 9.10 or newer. It exports the STG IR `(.modpak)` for the compiled modules and linker metadata (`.ghc_stgapp`) at application link time.  

<img height="350" src="https://user-images.githubusercontent.com/877489/114280753-0d311300-9a3b-11eb-8d50-facad35f0e9a.png"/>

## Presentation video
- [Why a GHC Whole Program Compiler Mode Would Be Useful](https://www.youtube.com/watch?v=lAoGF0jlxKI) *([slides](https://docs.google.com/presentation/d/18N8UOw-bexpoKlLLRHzSX-tY-yrbTnA6PQ9WtAVgUV4/edit?usp=sharing))*
- [Why and How the External STG Interpreter is Useful](https://www.youtube.com/watch?v=wt6iCgYmVGA) *([slides](https://docs.google.com/presentation/d/1Lmfpwtx_7TbIAGYnSE0HqkawRu75y2GGwbObuu0xYPY/edit#slide=id.p))* *([demo code](https://github.com/grin-compiler/ext-stg-interpreter-presentation-demos))*

## Readings
- [Introducing GHC whole program compiler (GHC-WPC)](https://www.patreon.com/posts/introducing-ghc-38173710)
- [GHC-WPC thread on ghc-devs mailing list](https://mail.haskell.org/pipermail/ghc-devs/2020-June/018994.html)
- [External STG Interpreter](https://www.patreon.com/posts/external-stg-49857800)

## External STG tools (Ext-STG)
- `gen-exe` main compiler driver, it produces executable from `.ghc_stgapp` files.
- `gen-obj` compiles STG IR files `.o_stgbin` to object code `.o`. (gen-exe calls it)
- `ext-stg` CLI tool for external STG IR, it can pretty print `.o_stgbin` files.

## Why?
- to make it easy to develop new backends for GHC without extending Cabal with new targets
- to facilitate compiler/PL research that needs real world programs to analyse
- to allow whole program analysis (new insights might be adopted to incremental compilers)  
- to escape from GHC codebase to the mainstream Haskell UX/DX that allows to use any library
- to allow program observation with arbitrary precision
- to make it easy to focus on the compiler backend development without hacking GHC
- to allow other compilers to target GHC/STG and the feature rich RTS 

## Build
#### external stg tooling
   ```
   stack install
   ```
#### `wpc-plugin`

1. Install [zip-cmd](https://hackage.haskell.org/package/zip-cmd), a simple CLI for the `zip` package
   ```
   cabal install zip-cmd
   ```
2. Compile the `wpc-plugin`
   The `wpc-plugin` has a speparate `stack.yaml` because it uses the plugin API of `GHC 9.10.3`.  
   ```
   cd wpc-plugin
   stack build
   ```
3. Find the built `libwpc-plugin.[so|dylib|dll]`
   ```
   ln -s `find . -type f -name 'libwpc-plugin.so' -o -name 'libwpc-plugin.dylib' -o -name 'libwpc-plugin.dll' | head -1`
   ```

## Usage
It is required to use GHC 9.10.3.

#### cabal
Add the following lines to your project's `cabal.project`:
```
package *
  ghc-options:
    -fplugin-trustworthy
    -fplugin-library=PATH_TO/libwpc-plugin.so;wpc-plugin-unit;WPC.Plugin;[]
```

#### stack
Add the following lines to your project's `stack.yaml`:
```
apply-ghc-options: everything
ghc-options:
  "$everything":
      -fplugin-trustworthy
      -fplugin-library=PATH_TO/libwpc-plugin.so;wpc-plugin-unit;WPC.Plugin;[]
```

## TODO
**Ext-STG IR**
- export IdInfo (without it `gen-exe` compiles -O0 executables)

## UnZip with Zstd support
The `.modpak` and `.fullpak` files use Zstd compression method that was introduced in the Zip 6.3.8 standard in 2020.  
The GHC-WPC tooling can handle Zstd zip files out of the box.  
But if you'd like to unpack the `.modpak` and `.fullpak` files manually then you'll need an `unzip` version with Zstd support.  
https://github.com/csabahruska/unzip-zstd
