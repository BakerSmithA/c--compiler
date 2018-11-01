module Back.ASMSpec where

import Test.Hspec
import qualified Front.AST as AST
import Back.ASM
import Back.Env (runSt, empty)
import Back.Instr

sp :: RegIdx
sp = 14

bp :: RegIdx
bp =16

asmSpec :: Spec
asmSpec = describe "asm generation" $ do
    pushSpec
    popSpec
    defSpec

defSpec :: Spec
defSpec = describe "def" $ do
    it "generates asm for def int" $ do
        let ast = (AST.Def "x" (AST.DefInt (AST.Lit 2)))
            exp = [MoveI 0 2
                 , StoreIdx { r=0, base=bp, offset=0 }]
        runSt empty (stm ast) `shouldBe` exp

    it "increments offset after bp for multiple int definitions" $ do
        let def n x = (AST.Def n (AST.DefInt (AST.Lit x)))
            ast = AST.Comp (def "x" 2) (def "y" 3)
            exp = [MoveI 0 2
                 , StoreIdx { r=0, base=bp, offset=0 }
                 , MoveI 0 3
                 , StoreIdx { r=0, base=bp, offset=1 }]
        runSt empty (stm ast) `shouldBe` exp

    it "generates asm for def arr" $ do
        let elems = [AST.Lit 1, AST.Lit 2]
            ast   = (AST.Def "x" (AST.DefArr elems))
            exp   = [Move 0 sp -- Save start address.

                   , MoveI 0 1 -- Push elements of array.
                   , StoreIdx { r=0, base=sp, offset=0 }
                   , AddI sp sp 1
                   , MoveI 0 2
                   , StoreIdx { r=0, base=sp, offset=0 }
                   , AddI sp sp 1

                   , StoreIdx 0 bp 2] -- Save start index to stack in variable.

        runSt empty (stm ast) `shouldBe` exp

pushSpec :: Spec
pushSpec = describe "push" $ do
    it "generates asm" $ do
        let exp = [StoreIdx { r=1, base=sp, offset=0 }
                 , StoreIdx { r=3, base=sp, offset=1 }
                 , AddI sp sp 2]
        runSt empty (push [1, 3]) `shouldBe` exp

popSpec :: Spec
popSpec = describe "pop" $ do
    it "generates asm" $ do
        let exp = [LoadIdx { r=1, base=sp, offset=(-1)}
                 , LoadIdx { r=3, base=sp, offset=(-2)}
                 , SubI sp sp 2]
        runSt empty (pop [1, 3]) `shouldBe` exp
