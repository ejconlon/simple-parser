# simple-parser

Simple parser combinators with a focus on controlled backtracking (choice with lookahead) for better error reporting. Unlike Megaparsec, this backtracks by default, supports user-defined labels for error reporting, and is very generic in the kind of structures it can parse.

## License

This project is BSD-licensed. Some gnarly functions to parse numbers and such have been adapted from Megaparsec, which is also [BSD-licensed](https://github.com/mrkkrp/megaparsec/blob/master/LICENSE.md).
