module Main where

import Test.Hspec
import Front.ParserSpec
import Front.ASTSpec
import Back.ASMSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        astSpec
        asmSpec
