# External STG interpreter

The external STG interpreter is independent from GHC and GHC-WPC, but still can run real Haskell programs.  
It is an excellent laboratory to study the runtime behaviour of Haskell programs in detail.  
It is like a programmable debugger with good UX/DX.

## Setup & Build

1. Clone the project's git mega repository.
   ```
   git clone git@github.com:grin-compiler/ghc-whole-program-compiler-project.git
   ```
2. Build from the interpreter's folder
   ```
   cd ghc-whole-program-compiler-project/external-stg-interpreter
   stack install
   ```

## Example Usage

Run the sample hello world program: `ghc-rts-base.fullpak`  
The `.fullpak` is a zip file (using Zstd compression) that contains the IR for the whole Haskell program, i.e. haskell source, core, stg, cmm, asm.

```
cd ghc-whole-program-compiler-project/external-stg-interpreter/data
ext-stg-interpreter ghc-rts-base.fullpak
hello
hello
```

Check the content of the content of the `ghc-rts-base.fullpak`.
