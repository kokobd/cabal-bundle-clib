# cabal-bundle-clib

Bundling C/C++ projects in Cabal package made easy.

(*The code doesn't need to be in C or C++, it just has to expose a C ABI*)

## Usage

Add a custom setup stanza to your Cabal file:

```Cabal
custom-setup
  setup-depends:
      base
    , Cabal
    , cabal-bundle-clib
```

Replace your default `Setup.hs` with the following code:
```haskell
import           Development.CabalBundleCLib       (mainWithCLib)
import           Development.CabalBundleCLib.CMake (simpleCMakeBuilder)

main :: IO ()
main = mainWithCLib
  "cpp_proj" -- C project root directory, relative to package root directory
  ["mycpplib"] -- C library names
  ["."] -- Direct parent dirs for built libraries, relative to build root dir.
  simpleCMakeBuilder -- The builder
```
For now, we only provide one builder: `simpleCMakeBuilder`. This builder
basically calls `cmake --build`, and doesn't take care of `cmake install`.

**NOTE** External native libraries, such as `stdc++`, still need to be listed
in `extra-libraries` as normal.

The library is extensible. You can write your own builder if your C project
doesn't use CMake, or has some complex build process. The provided
`simpleCMakeBuilder` is a good example to get started.

Furthermore, there are several example packages provided.
- `example-app` An application that bundles C code.
- `example-lib` A library that bundled C Code.
- `example-lib-consumer` An application that depends on `example-lib`, but has
  no knowledge of the C code used in `example-lib`

### Compatibility

This library is tested against:
- Cabal and cabal-install 2.4.1.0, using v2 build
- stack 1.9.1 with `lts-13.0`
- cmake 3.13.2

## Motivation

Traditionally, there are two ways to organize a project that contains both
Haskell and C code
- List the C files in `c-sources` of Cabal file, and let cabal build them
- Build and install the C project manually (or use external package managers,
  such as `apt`), then list the relative installation path in `extra-lib-dirs`

However, neither way is perfect. With the first method, we are forced to let
Cabal build the C code, and you may want to use CMake, for example. And if you
are trying to bundle a third-party C library with its own build system, this
approach won't work. With the second method, the build process of the C project
is not managed by Cabal. You have to make sure the libraries are properly
installed on the system. What's worse, an application developer have to take
care of any C libraries used by the Haskell libraries he/she use, even
transitive dependencies. For example, to use the library `hmatrix`, whether
directly or indirectly, you will have to install `libblas` `liblapack`, etc.

So, we want an approach to bundle a C project inside a Cabal package. The
package developer can edit the C code at any time, and cabal should detect the
changes and trigger incremental build. Just as if he/she is editing Haskell
code. If the package is a library, then for consumers of the package, they do
not need to know there exists some C code in that library.

There is a [blog][1] providing some basic ideas of this approach. It makes the C
code transparent to library users. However, using the exact code posted in that
blog, the development process is not satisfactory. It builds the C code in
`configure` stage due to some 'limitations' of Cabal, which means `cabal
v2-build` will not trigger an incremental build of the C code. Based on that blog,
I improved the implementation to satisfy the requirements stated above.

[1]: https://codinginfinity.me/post/2015-04-18/haskell_and_cpp
