module Main (main) where

import Test.SimpleParser.SimpleTest (testSimple)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "SimpleParser" [testSimple])
