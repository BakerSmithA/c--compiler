module Main where

import Test.Hspec
import Front.ParserSpec
import Back.ASMSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        asmSpec
