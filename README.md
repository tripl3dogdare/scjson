# ScJson

ScJson is a lightweight, batteries-included library for dealing with JSON in Scala. It includes everything you need for basic JSON usage in Scala, including a pattern-matchable AST, concise syntax for both creating and reading JSON, and a fully spec-compliant parser.

ScJson is designed with the goal of coming as close as possible to the ease of managing JSON in dynamically typed languages like Javascript. While this makes it fast, lightweight, and easy to use, it does come with it's own drawbacks - for example, a reduction in type safety and lack of some features that might be considered "basic" when it comes to Scala JSON libraries.


## Installation

See [Getting Started](https://github.com/tripl3dogdare/scjson/wiki/Getting-Started).


## Why ScJson?

- Lightweight and batteries included
  - One source file weighing in at < 200 lines of code
  - No third-party dependencies, even for parsing
  - Only one import statement to remember
- Easy to use
  - No case-class destructuring required
  - Simple shortcut syntax for recursing into deeply nested JSON
  - Longer, more explicit syntax options for dealing with JSON in a type-safe manner
  - Concise DSL syntax for creating JSON
  - Extensive implicit conversions reduce boilerplate from boxing/unboxing values
- Simple, effective structure
  - DSL syntax is provided by only 3 top level functions, excluding implicit conversions
  - Consistent naming and symmetric object models make dealing with different types simple
  - JSON objects can be converted to valid JSON strings with a single method call


## Why not ScJson?

- No case class destructuring
- Error messages are rather cryptic
- Limited type and null safety
- Lacking in documentation

All of the above problems are being worked on as time goes on, except case class destructuring which is a "maybe" feature rather than an "eventually" feature.