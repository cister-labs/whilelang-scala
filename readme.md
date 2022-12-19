# WhileLang

Small `While` language to be used in an introductory course on software verification.
It produces JavaScript code that can be executed at the link below, using a snapshot of this tool:

 - https://cister-labs.github.io/whilelang-scala


## Requirements

- JVM (>=1.8)
- sbt

## Compilation

You need to get the submodules dependencies (CAOS library), and later compile using ScalaJS.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `git submodule update --init`
2. `sbt fastLinkJS`
3. open the file `lib/tool/index.html`
