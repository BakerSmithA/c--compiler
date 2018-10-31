module Main where

import Test.Hspec
import Front.ParserSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
