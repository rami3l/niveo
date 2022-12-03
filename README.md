# Niveo

```txt
 _  _  __
| \| | \ \)  ___  ___
| .  | /\ \_/ ._)/ . \
|_|\_|/_/\_/\___|\___/

```

A minimalistic programmable configuration language.

> Niveo is still in very early stages and not ready for production.
> It currently serves as a language design practice for BUAA's _Programming Language Principle_ course.

---

## Overview

**Functional**, **expression-based** and dynamically-typed, Niveo tries to capture the essential of what you might expect from a simple configuration DSL which is easy to port and extend.

With a familiar C-like syntax, Niveo helps you make sense of the configuration from the first glance.

```re
letrec fact = fun(a) {
    if (a <= 1) 1 else a * fact(a - 1)
};
struct{"fact(5)" = fact(5)}
```

Support for programming language constructs like bindings and functions can help reduce duplications and improve configuration reusability.

```re
let ssh_info = fun(username) {
    let home = "/home/" + username;
    let privateKey = home + "/.ssh/id_ed25519";
    struct{
        home,
        privateKey,
        publicKey: privateKey + ".pub",
    }
};
ssh_info("bill")
```

For more concrete examples, please check out the [integration tests](test/Tests/Interpreter.hs).

## CLI Usage

```txt
niveo - A minimalistic programmable configuration language.

Usage: niveo [(-f|--run FILE) | (-c|--cmd SCRIPT)] [--export-json|--json]

Available options:
  -f,--run FILE            Load and interpret FILE
  -c,--cmd SCRIPT          Interpret the given SCRIPT string
  --export-json,--json     Enable JSON output
  -h,--help                Show this help text
```

Without any flags from the `-f/-c` group, the Niveo REPL will be launched.

## Try it out!

With the latest [Haskell toolchain](https://www.haskell.org/downloads) installed (preferably with [`ghcup`](https://www.haskell.org/ghcup)), just execute:

```sh
# To launch the REPL:
cabal run

# To add more flags, e.g. to run in one-off mode:
cabal run niveo-exe -- -c 'let one = 1; struct{result: one + one}' --json
```
