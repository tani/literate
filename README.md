# Literate (Lean 4)

`literate` is a small Lean 4 library for literate-programming style source files.
It allows markdown-like prose in Lean files while executing explicit `~~~lean` fenced blocks.
This is a practical example of a [polyglot](https://en.wikipedia.org/wiki/Polyglot_(computing)) source style.

## Features

- `~~~lean ... ~~~` command blocks are elaborated as normal Lean commands.
- Markdown heading lines like `# Heading text` are accepted and ignored.
- Plain prose lines composed of identifiers are accepted and ignored.

## Install

Add this package to your `lakefile.toml`:

```toml
[[require]]
name = "literate"
git = "https://github.com/tani/literate.git"
```

Then import it:

```lean
import Literate
```

## Usage

```lean
import Literate

# This heading is ignored

This line is prose and is ignored

~~~lean
namespace Demo

def success := "This was evaluated!"
#check success

end Demo
~~~
```

## Development

```bash
lake build
lake env lean Literate/Examples/Basic.lean
```

## Status

Early version (`0.1.0`). Expect syntax/behavior changes.
