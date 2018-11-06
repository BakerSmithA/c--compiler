module Back.ASMSpec where

import Test.Hspec
import qualified Front.AST as AST
import Back.ASM
import Back.Env (Env)
import Back.Env as Env (runSt, empty, fromVars, takeEnvReg)
import Back.Instr

sp :: RegIdx
sp = 14

bp :: RegIdx
bp = 16

-- Take register from empty environment.
takeEmptyEnv :: Env
takeEmptyEnv = snd (Env.takeEnvReg Env.empty)

asmSpec :: Spec
asmSpec = describe "asm generation" $ do
    pushSpec
    popSpec
    intValSpec
    intValAllSpec
    boolValSpec
    defSpec
    assignSpec
    assignArrElemSpec
    ifSpec
    ifElseSpec
    whileSpec
    forSpec
    callSpec

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
            env = takeEmptyEnv
            exp = [MoveI 0 5]
        runSt env (intVal ast 0) `shouldBe` exp

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
            env = takeEmptyEnv
            exp = [MoveI 0 2
                 , MoveI 2 5
                 , Add 0 0 2
                 , MoveI 1 3
                 , Sub 0 0 1]
        runSt env (intVal ast 0) `shouldBe` exp

intValAllSpec :: Spec
intValAllSpec = describe "intValAllSpec" $ do
    it "generates asm for each val" $ do
        let val0 = AST.Lit 2
            val1 = AST.Lit 3
            val2 = AST.Var "x"
            env = Env.fromVars [("x", 7)]
            exp = [MoveI 5 2
                 , MoveI 3 3
                 , LoadIdx { r=1, base=bp, offset=7 }]
            vals = [(val0, 5), (val1, 3), (val2, 1)]
        runSt env (intValAll vals) `shouldBe` exp

boolValSpec :: Spec
boolValSpec = describe "bool value" $ do
    it "generates asm for true" $ do
        let ast = AST.TRUE
            env = takeEmptyEnv
            exp = [MoveI 0 1]
        runSt env (boolVal ast 0) `shouldBe` exp

    it "generates asm for false" $ do
        let ast = AST.FALSE
            env = takeEmptyEnv
            exp = [MoveI 0 0]
        runSt env (boolVal ast 0) `shouldBe` exp

    it "generates asm for eq" $ do
        let ast = AST.Eq (AST.Lit 1) (AST.Lit 2)
            env = takeEmptyEnv
            exp = [MoveI 0 1
                 , MoveI 1 2
                 , Eq 0 0 1]
        runSt env (boolVal ast 0) `shouldBe` exp

    it "generates asm for not equal" $ do
        let ast = AST.NEq (AST.Lit 1) (AST.Lit 2)
            env = takeEmptyEnv
            exp = [MoveI 0 1
                 , MoveI 1 2
                 , Eq 0 0 1
                 , Not 0 0]
        runSt env (boolVal ast 0) `shouldBe` exp

    it "generates asm for less-than" $ do
        let ast = AST.Lt (AST.Lit 1) (AST.Lit 2)
            env = takeEmptyEnv
            exp = [MoveI 0 1
                 , MoveI 1 2
                 , Lt 0 0 1]
        runSt env (boolVal ast 0) `shouldBe` exp

    it "generates asm for greater-than" $ do
        let ast = AST.Gt (AST.Lit 1) (AST.Lit 2)
            env = takeEmptyEnv
            exp = [MoveI 0 2
                 , MoveI 1 1
                 , Lt 0 0 1]
        runSt env (boolVal ast 0) `shouldBe` exp

    it "generates asm for or" $ do
        let ast = AST.Or AST.FALSE AST.TRUE
            env = takeEmptyEnv
            exp = [MoveI 0 0
                 , MoveI 1 1
                 , Or 0 0 1]
        runSt env (boolVal ast 0) `shouldBe` exp

    it "generates asm for and" $ do
        let ast = AST.And AST.FALSE AST.TRUE
            env = takeEmptyEnv
            exp = [MoveI 0 0
                 , MoveI 1 1
                 , And 0 0 1]
        runSt env (boolVal ast 0) `shouldBe` exp

defSpec :: Spec
defSpec = describe "def" $ do
    it "generates asm for def int" $ do
        let ast = AST.Def "x" (AST.DefInt (AST.Lit 2))
            exp = [MoveI 0 2
                 , StoreIdx { r=0, base=sp, offset=0 }
                 , AddI sp sp 1]
        runSt empty (stm ast) `shouldBe` exp

    it "overwrites existing var if already exists" $ do
        let def n x = AST.Def n (AST.DefInt (AST.Lit x))
            ast = AST.Comp [def "x" 2, def "x" 3]
            exp = [MoveI 0 2
                 , StoreIdx { r=0, base=sp, offset=0 }
                 , AddI sp sp 1
                 -- Declare same variable again should not cause a new variable
                 -- to be pushed to stack.
                 , MoveI 0 3
                 , StoreIdx { r=0, base=bp, offset=0 }]
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
            ast = AST.Comp [def "x" 2, def "y" 3, assign "y" 4]
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

forSpec :: Spec
forSpec = describe "for" $ do
    it "generates asm" $ do
        let range = AST.IntRange (AST.Lit 1) (AST.Lit 5)
            ast   = AST.For "i" range (AST.Print (AST.Var "i"))
            exp   = [MoveI { r=0, val=1 } -- Save variable i initial value.
                   , StoreIdx { r=0, base=sp, offset=0 }
                   , AddI sp sp 1

                   -- Compare i to upper bound.
                   , LoadIdx { r=0, base=bp, offset=0 }
                   , MoveI { r=1, val=5 }
                   , Lt 0 0 1

                   -- Branch to end if i is already above upper bound.
                   , BF 0 "1"

                   -- Body of loop.
                   , Label "0"
                   , LoadIdx { r=1, base=bp, offset=0 }
                   , Print 1

                   -- Update i.
                   , LoadIdx { r=1, base=bp, offset=0 }
                   , MoveI { r=2, val=1 }
                   , Add 1 1 2
                   , StoreIdx { r=1, base=bp, offset=0 }

                   -- Check whether i is past upper bound. Branch to start if not.
                   , LoadIdx { r=0, base=bp, offset=0 }
                   , MoveI { r=1, val=5 }
                   , Lt 0 0 1
                   , BT 0 "0"

                   -- Exit
                   , Label "1"]
        runSt empty (stm ast) `shouldBe` exp

callSpec :: Spec
callSpec = describe "call" $ do
    it "generates asm" $ do
        let ast = AST.Call (AST.FuncCall "func" [AST.Lit 3, AST.Var "x", AST.Lit 7])
            env = Env.fromVars [("x", 0)]
            exp = []
        runSt env (stm ast) `shouldBe` exp
