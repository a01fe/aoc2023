# AoC2023

My attempts at solving [Advent of Code 2023](https://adventofcode.com/2023) while
learning [Scala](https://www.scala-lang.org/).
Please excuse my awkward code, I'm still getting my Scala legs =^.^=

## Environment

The environment I'm using is:

* macOS 14 on a 16" M1 MBP
* Temurin OpenJDK 21 installed via MacPorts
* Visual Studio Code (insiders)
* Metals VSC extension
* [scala-cli](https://scala-cli.virtuslab.org/)

To initialize a new clone, run this inside the project:

```bash
scala-cli setup-ide .
scala-cli compile .
```

After doing that, Scala worksheets should automatically recalculate as you change
them in VSCode.

Each day's puzzle is in a Scala worksheet in `src/main/scala`.
Copies of my input files are in `inputs`.
