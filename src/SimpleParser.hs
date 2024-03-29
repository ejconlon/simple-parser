-- | This is basically a simpler and slower (Mega)Parsec that is fully backtracking by default.
--
-- The root module re-exports all modules. See "SimpleParser.Examples.Json" or the test suit for examples,
-- "SimpleParser.Parser" for the core transformer, "SimpleParser.Stream" for the source abstraction,
-- or "SimpleParser.Input" for useful combinators.
module SimpleParser
  ( module SimpleParser.CharString
  , module SimpleParser.Chunked
  , module SimpleParser.Common
  , module SimpleParser.Explain
  , module SimpleParser.Input
  , module SimpleParser.Interactive
  , module SimpleParser.Lexer
  , module SimpleParser.LookAhead
  , module SimpleParser.Parser
  , module SimpleParser.Result
  , module SimpleParser.Stack
  , module SimpleParser.Stream
  , module SimpleParser.Throw
  )
where

import SimpleParser.CharString
import SimpleParser.Chunked
import SimpleParser.Common
import SimpleParser.Explain
import SimpleParser.Input
import SimpleParser.Interactive
import SimpleParser.Lexer
import SimpleParser.LookAhead
import SimpleParser.Parser
import SimpleParser.Result
import SimpleParser.Stack
import SimpleParser.Stream
import SimpleParser.Throw
