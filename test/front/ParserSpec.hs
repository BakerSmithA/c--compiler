module Front.ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (runParser)
import Front.AST
import Front.Parser (funcDefs)

lit :: Int -> IntVal
lit x = Lit x

var :: VarName -> IntVal
var name = Var name

result :: FuncCall -> IntVal
result call = Result call

noOp :: Stm
noOp = Comp []

parserSpec :: Spec
parserSpec = do
    describe "parser" $ do
        context "parses functions with" $ do
            it "no args or statements" $ do
                let s = "def f() {}"
                    exp = FuncDef "f" [] Nothing noOp
                runParser funcDefs "" s `shouldParse` [exp]

            it "return type" $ do
                let s = "def f() -> Int {}"
                    exp = FuncDef "f" [] (Just IntType) noOp
                runParser funcDefs "" s `shouldParse` [exp]

            it "args" $ do
                let s = "def f(x: Int, y: Int[3]) {}"
                    exp = FuncDef "f" [TypedVar "x" IntType, TypedVar "y" (ArrType IntType (Just 3))] Nothing noOp
                runParser funcDefs "" s `shouldParse` [exp]

            it "body with single statement" $ do
                let s = "def f() { return 0 }"
                    exp = FuncDef "f" [] Nothing (Return (lit 0))
                runParser funcDefs "" s `shouldParse` [exp]

            it "body with multiple statements" $ do
                let s = "def f() { let x = 1 \n return x }"
                    exp = FuncDef "f" [] Nothing (Comp [Def "x" (DefInt (lit 1)), Return (var "x")])
                runParser funcDefs "" s `shouldParse` [exp]

            it "body with call assignment" $ do
                let s = "def f() { let x = f() }"
                    exp = FuncDef "f" [] Nothing (Def "x" (DefInt (result (FuncCall "f" []))))
                runParser funcDefs "" s `shouldParse` [exp]

            it "body with call" $ do
                let s = "def f() { f() }"
                    exp = FuncDef "f" [] Nothing (Call (FuncCall "f" []))
                runParser funcDefs "" s `shouldParse` [exp]

            it "parses int ops" $ do
                let s = "def f() { return 1 + 2 - 3}"
                    exp = FuncDef "f" [] Nothing (Return (Sub (Add (Lit 1) (Lit 2)) (Lit 3)))
                runParser funcDefs "" s `shouldParse` [exp]

        context "parses program with" $ do
            it "multiple functions" $ do
                let s = "def f() {} \n def main() {}"
                    exp = [FuncDef "f" [] Nothing noOp, FuncDef "main" [] Nothing noOp]
                runParser funcDefs "" s `shouldParse` exp
