module Back.ASMSpec where

import Test.Hspec
import qualified Front.AST as AST
import Back.ASM
import Back.Env as Env (runSt, empty, fromVars, takeEnvReg)
import Back.Instr

sp :: RegIdx
sp = 14

bp :: RegIdx
bp = 16

asmSpec :: Spec
asmSpec = describe "asm generation" $ do
    pushSpec
    popSpec
    intValSpec
    defSpec
    assignSpec
    assignArrElemSpec
    ifSpec
    ifElseSpec
    whileSpec

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

intValSpec :: Spec
intValSpec = describe "int val" $ do
    it "generates asm for variable" $ do
        let ast = AST.Var "x"
            (reg, env) = Env.takeEnvReg (Env.fromVars [("x", 5)])
            exp = [LoadIdx { r=reg, base=bp, offset=5 }]
        runSt env (intVal ast reg) `shouldBe` exp

    it "generates asm for literal" $ do
        let ast = AST.Lit 5
            exp = [MoveI 0 5]
        runSt empty (intVal ast 0) `shouldBe` exp

    it "generates asm for function result" $ do
        pending

    it "generates asm for array access" $ do
        let ast = AST.ArrAccess "xs" (AST.Lit 5)
            (reg, env) = Env.takeEnvReg (Env.fromVars [("xs", 7)])
            exp = [LoadIdx { r=reg, base=bp, offset=7 }
                 , MoveI 1 5
                 , LoadBaseIdx { r=reg, base=0, rOffset=1 }]
        runSt env (intVal ast reg) `shouldBe` exp

    it "generates asm for add and sub" $ do
        let ast = AST.Sub (AST.Add (AST.Lit 2) (AST.Lit 5)) (AST.Lit 3)
            exp = [MoveI 0 2
                 , MoveI 1 5
                 , Add 0 0 1
                 , MoveI 1 3
                 , Sub 0 0 1]
        runSt empty (intVal ast 0) `shouldBe` exp

defSpec :: Spec
defSpec = describe "def" $ do
    it "generates asm for def int" $ do
        let ast = AST.Def "x" (AST.DefInt (AST.Lit 2))
            exp = [MoveI 0 2
                 , StoreIdx { r=0, base=sp, offset=0 }
                 , AddI sp sp 1]
        runSt empty (stm ast) `shouldBe` exp

    it "generates asm for def arr" $ do
        let elems = [AST.Lit 1, AST.Lit 2]
            ast   = (AST.Def "x" (AST.DefArr elems))
            exp   = [Move 0 sp -- Save start address.

                   , MoveI 1 1 -- Push elements of array.
                   , StoreIdx { r=1, base=sp, offset=0 }
                   , AddI sp sp 1
                   , MoveI 1 2
                   , StoreIdx { r=1, base=sp, offset=0 }
                   , AddI sp sp 1

                   , StoreIdx { r=0, base=sp, offset=0 } -- Store start of array address on stack.
                   , AddI sp sp 1]

        runSt empty (stm ast) `shouldBe` exp

assignSpec :: Spec
assignSpec = describe "assign" $ do
    it "generates asm to reassign variable on stack" $ do
        let def n x = AST.Def n (AST.DefInt (AST.Lit x))
            assign n x = AST.Assign n (AST.DefInt (AST.Lit x))
            ast = AST.Comp (AST.Comp (def "x" 2) (def "y" 3)) (assign "y" 4)
            exp = [MoveI 0 2
                 , StoreIdx { r=0, base=sp, offset=0 }
                 , AddI sp sp 1

                 , MoveI 0 3
                 , StoreIdx { r=0, base=sp, offset=0 }
                 , AddI sp sp 1

                 , MoveI 0 4
                 , StoreIdx { r=0, base=bp, offset=1 }]
        runSt empty (stm ast) `shouldBe` exp

assignArrElemSpec :: Spec
assignArrElemSpec = describe "assign array elem" $ do
    it "generates asm to reassign array elem" $ do
        let idx = AST.Lit 2
            val = AST.Lit 5
            ast = AST.AssignArr "xs" idx val
            env = Env.fromVars [("xs", 10)]
            exp = [LoadIdx { r=0, base=bp, offset=10 }
                 , MoveI 1 2
                 , MoveI 2 5
                 , StoreBaseIdx { r=2, base=0, rOffset=1 }]
        runSt env (stm ast) `shouldBe` exp

ifSpec :: Spec
ifSpec = describe "if" $ do
    it "generates asm" $ do
        let ast = AST.If AST.FALSE (AST.Print (AST.Lit 5))
            exp = [MoveI 0 0
                 , BF 0 "0"
                 , MoveI 1 5
                 , Print 1
                 , Label "0"]
        runSt empty (stm ast) `shouldBe` exp

ifElseSpec :: Spec
ifElseSpec = describe "if else" $ do
    it "generates asm" $ do
        let ast = AST.IfElse AST.TRUE (AST.Print (AST.Lit 5)) (AST.Print (AST.Lit 8))
            exp = [MoveI 0 1
                 , BF 0 "0"
                 , MoveI 1 5
                 , Print 1
                 , B "1"
                 , Label "0"
                 , MoveI 1 8
                 , Print 1
                 , Label "1"]
        runSt empty (stm ast) `shouldBe` exp

whileSpec :: Spec
whileSpec = describe "while" $ do
    it "generates asm" $ do
        let ast = AST.While AST.FALSE (AST.Print (AST.Lit 5))
            exp = [MoveI 0 0
                 , BF 0 "1"
                 , Label "0"
                 , MoveI 1 5
                 , Print 1
                 , MoveI 0 0
                 , BT 0 "0"
                 , Label "1"]
        runSt empty (stm ast) `shouldBe` exp
