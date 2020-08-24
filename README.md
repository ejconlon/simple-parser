# simple-parser

[![CircleCI](https://circleci.com/gh/ejconlon/simple-parser/tree/master.svg?style=svg)](https://circleci.com/gh/ejconlon/simple-parser/tree/master)

Simple parser combinators following the clever refrain (by Fritz Ruehr?)

    A parser for things
    Is a function from strings
    To lists of pairs
    Of things and strings.

In this case, we subsitute `ListT` for the list and add some error handling. We also swap out strings for any kind of input (streaming or not).
