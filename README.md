# LiterateLean for Lean 4


`literate-lean` is a small Lean 4 library for literate-programming style source files.
It allows markdown-like prose in Lean files while executing explicit `~~~lean` fenced blocks.
This is a practical example of a [polyglot](https://en.wikipedia.org/wiki/Polyglot_(computing)) source style.

<img width="1552" height="1012" alt="image" src="https://github.com/user-attachments/assets/6534c42a-7009-4117-9ace-b92ea7afa69b" />


## Features

- `~~~lean ... ~~~` command blocks are elaborated as normal Lean commands.
- Markdown heading lines like `# Heading text` are accepted and ignored.
- Plain prose lines composed of identifiers are accepted and ignored.

## Install

Add this package to your `lakefile.toml`:

```toml
[[require]]
name = "LiterateLean"
git = "https://github.com/tani/literate-lean.git"
```

Then import it:

```lean
import LiterateLean
```

## Usage

```lean
    import LiterateLean

# This heading is ignored

This line is prose and is ignored

~~~lean
namespace Demo

def success := "This was evaluated!"
#check success

end Demo
~~~
```

>     import LiterateLean
> 
> # This heading is ignored
> 
> This line is prose and is ignored
> 
> ~~~lean
> namespace Demo
> 
> def success := "This was evaluated!"
> #check success
> 
> end Demo
> ~~~

## Development

```bash
lake build
lake env lean LiterateLean/Examples/Basic.lean
```

## Status

Early version (`0.1.0`). Expect syntax/behavior changes.

## Copyright

Copyright (c) 2025 Taniguchi Masaya. All Right Reserved.
