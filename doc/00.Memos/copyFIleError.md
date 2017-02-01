
```
$ stack build
scheme48-0.1.0.0: build (lib + exe)
Preprocessing library scheme48-0.1.0.0...
Preprocessing executable 'scheme48-exe' for scheme48-0.1.0.0...
[1 of 1] Compiling Main             ( app/Main.hs, .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/scheme48-exe/scheme48-exe-tmp/Main.o )
Linking .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/scheme48-exe/scheme48-exe ...

Warning: The following modules should be added to exposed-modules or other-modules in /Users/iyahoo/Dropbox/Program/Haskell/scheme48/scheme48.cabal:
    - In scheme48-test component:
        LibSpec

Missing modules in the cabal file are likely to cause undefined reference errors from the linker, along with other problems.
scheme48-0.1.0.0: copy/register
LICENSE: copyFile: does not exist (No such file or directory)
'cabal copy' failed.  Error message:

--  While building package scheme48-0.1.0.0 using:
      /Users/iyahoo/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_1.24.0.0_ghc-8.0.1 --builddir=.stack-work/dist/x86_64-osx/Cabal-1.24.0.0 copy
    Process exited with code: ExitFailure 1

One possible cause of this issue is:
* No module named "Main". The 'main-is' source file should usually have a header indicating that it's a 'Main' module.
```

LICENSE ファイルが無いといけないらしい。CC の LICENSE ファイルが見つからないのでとりあえずファイルだけ作成した。

