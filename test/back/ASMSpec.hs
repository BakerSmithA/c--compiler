module Back.ASMSpec where

import Test.Hspec
import Back.ASM
import Back.Env (runSt, empty)
import Back.Instr

sp :: RegIdx
sp = 14

asmSpec :: Spec
asmSpec = describe "asm generation" $ do
    pushSpec

pushSpec :: Spec
pushSpec = describe "push" $ do
    it "generates asm" $ do
        let exp = [StoreIdx { r=1, base=sp, offset=0 }
                 , StoreIdx { r=3, base=sp, offset=1 }
                 , AddI sp sp 2]
        runSt empty (push [1, 3]) `shouldBe` exp
