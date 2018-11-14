module Front.ASTSpec where

import Test.Hspec
import Front.AST

astSpec :: Spec
astSpec = describe "ast" $
    defsSpec

defsSpec :: Spec
defsSpec = describe "defs" $ do
    it "returns all definitions in order declared" $ do
        let def n x = Def n (DefInt (Lit x))
            ast = Comp [def "x" 1, def "y" 2, PrintLn, def "z" 3]
            exp = [("x", DefInt (Lit 1)), ("y", DefInt (Lit 2)), ("z", DefInt (Lit 3))]
        defs ast `shouldBe` exp

    it "finds nested definitions" $ do
        let ast = If TRUE (Def "x" (DefInt (Lit 1)))
            exp = [("x", DefInt (Lit 1))]
        defs ast `shouldBe` exp
