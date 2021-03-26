-- | This is basically a simpler and slower (Mega)Parsec that is fully backtracking by default.
--
-- The root module re-exports all modules. See "SimpleParser.Examples.Json" or the test suit for examples,
-- "SimpleParser.Parser" for the core transformer, "SimpleParser.Stream" for the source abstraction,
-- or "SimpleParser.Input" for useful combinators.
module SimpleParser
  ( module SimpleParser.Bundle
  , module SimpleParser.Chunked
  , module SimpleParser.Common
  , module SimpleParser.Input
  , module SimpleParser.Labels
  , module SimpleParser.Parser
  , module SimpleParser.Result
  , module SimpleParser.Stream
  ) where

import SimpleParser.Bundle
import SimpleParser.Chunked
import SimpleParser.Common
import SimpleParser.Input
import SimpleParser.Labels
import SimpleParser.Parser
import SimpleParser.Result
import SimpleParser.Stream
