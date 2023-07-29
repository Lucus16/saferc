# The SaferC Programming Language

## Goals

The SaferC language is intended to be a safer alternative to C for systems
programming, with a focus on high compatibility with existing C code. In order
to make it easy to build this language and to ensure compatibility, the compiler
will generate readable C code, including the original comments from the SaferC
source. It will not have backwards compatible syntax, instead opting for a
syntax that is simpler to parse. It will also not be able to parse C header
files. Instead, the user will write interfaces for each C library that specify
that interface more completely.

## Planned features

- Pointers are explicitly made nullable. Nullable pointers must be checked for
  nullity before being used.
- Size parameters, struct fields and locals can be used to specify the number of
  elements that a pointer points to and this is checked. Multiple pointers may
  refer to the same size variable.
- Index parameters can specify which array they index or which size they are
  smaller than.
- owned vs borrowed reference analysis
- unique vs shared reference analysis
- Partially initialized structs must be explicitly marked as such. The compiler
  does its best to disallow use before initialization at compile time. If the
  compiler can't prove it, it's a compile time error.
- Bitfields are explicitly part of a carrying integer type and cannot cross
  multiple integer types.
- Range analysis to disallow potential integer overflows at compile time.
- Pure functions can be marked as such and cannot do IO, access globals or write
  to shared references.

## Plan for implementation

The initial compiler will be written in Haskell. Once the language's safety
features are starting to become reliable, work on bootstrapping can start.

1. Build a parser for something similar to C but easier to parse and print
   corresponding C code.
2. Add syntax for nullable types and check nullability.
3. ...

## Long term goals

- A backend that generates machine code in addition to the one generating C
  code.
- A tool to generate SaferC from existing C code. The generated code may not
  compile until the user finishes the specification needed to check everything
  that SaferC wants to check.
