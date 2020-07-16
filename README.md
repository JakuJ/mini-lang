# THE compiler for *mini++*

This repository contains the final project for the course on compilers at MiNI PW.

**Disclaimer:** It was part of the requirements that all the logic is in one `.cs` file. I've tried my best to make it readable though :eyes:

# Requirements

- `gplex` and `gppg` parser generators to process the `.lex` and `.y` files
- A C# 7.3 compiler

It's probably easiest to just open the solution in an IDE like **Rider** or **Visual Studio** and build using that.

This project was exclusively developed on Mac OS – if you're using Windows, switch to the **windows** branch.
That's where the version that works under Visual Studio 2017 resides.

## Caveats

You need to to add a reference to the `QUT.ShiftReduceParser.dll` library – you either already have it as it came with `gppg`, or you need to build it [from source](https://github.com/k-john-gough/gppg).

You also have to generate the `Scanner.cs` and `Parser.cs` files using `gplex` and `gppg` before build. I've set it up as a build step, but the binary paths are hard-coded, so you need to either change them or do it manually.

# Unit tests

The project is accompanied with a range of **NUnit**-based unit tests.
Combined with code coverage analysis, they were incredibly useful at diagnosing bugs.

The tests are divided into three groups:
- [source files](./UnitTests/TestSources/Valid) meant to be [successfully compiled](./UnitTests/ValidPrograms.cs)
- [source files](./UnitTests/TestSources/Outputs) meant to compile and print out the [correct output](./UnitTests/OutputPrograms.cs)
- [source files](./UnitTests/TestSources/Invalid) meant to [fail to compile](./UnitTests/InvalidPrograms.cs)

The compiler has also been repeatedly tested with the [**community sourced tests**](https://github.com/piotr-onyszczuk/MT_testy).
