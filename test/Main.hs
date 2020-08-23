module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Simpleparser.SimpleTest (testSimple)

main :: IO ()
main = defaultMain (testGroup "Simpleparser" [testSimple])
