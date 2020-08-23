module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.SimpleParser.SimpleTest (testSimple)

main :: IO ()
main = defaultMain (testGroup "SimpleParser" [testSimple])
