module Back.ASM where

import qualified Front.AST as AST
import Back.Env (St)
import qualified Back.Env as Env
import Back.Instr
import Control.Monad.State (get)

-- Takes result register and two operand registers.
type BinOp = RegIdx -> RegIdx -> RegIdx -> Instr

prog :: [AST.FuncDef] -> St [Instr]
prog [AST.FuncDef _ _ _ body] = stm body

-- prog funcs =
--     let (setup, env) = setupAsm Env.empty
--     in setup ++ funcDefs funcs env ++ [SysCall]

-- setupAsm :: ASMEnv -> ([Instr], ASMEnv)
-- setupAsm env = (pushLrAsm ++ callMainAsm, env) where
--     callMainAsm = [B "main"]
--     pushLrAsm = [MoveLabel linkReg EndAddr] -- After main returns, jump to end of program.
--     linkReg = Env.lrIdx env

-- funcDefs :: [FuncDef] -> ASMEnv -> [Instr]
-- funcDefs funcs env = fst $ foldl f ([], env) funcs where
--     f :: ([Instr], ASMEnv) -> FuncDef -> ([Instr], ASMEnv)
--     f (instrs, env) (FuncDef name args retType body) =
--         let argNames = map varName args
--             (instrs', env') = stm body (Env.inFunc argNames env)
--         in (instrs ++ [I.Label name] ++ instrs' ++ [I.Ret], Env.restore env env')

stm :: AST.Stm -> St [Instr]
stm (AST.Return val) = ret val
stm (AST.Def name val) = def name val
stm (AST.Assign name val) = assign name val
stm (AST.AssignArr name arrIdx val) = assignArr name arrIdx val
stm (AST.If cond body) = ifAsm cond body
stm (AST.IfElse cond sThen sElse) = ifElse cond sThen sElse
stm (AST.For name range body) = undefined
stm (AST.While cond body)= while cond body
stm (AST.Call func) = call func
stm (AST.Comp s1 s2) = comp s1 s2
stm (AST.NoOp) = return []
stm (AST.Print val) = printAsm val
stm (AST.PrintLn) = println

-- Calculates int value and populates return register with it.
ret :: AST.IntVal -> St [Instr]
ret val = do
    reg <- Env.ret
    intVal val reg

-- Calculates int value and stores on stack. Also advances stack pointer.
-- E.g. Before def x=1
--      -----------
--      |    1    |
--      |    4    |
--      |         | <- SP
--      -----------
--
-- After:
--      -----------
--      |    1    |
--      |    4    |
--      |    X    |
--      |         | <- SP
--      -----------
def :: AST.VarName -> AST.DefVal -> St [Instr]
def name val = Env.tempReg $ \reg -> do
    sp <- Env.sp
    valAsm <- defVal val reg
    pushVal <- push [reg]
    bpOffset <- fmap Env.bpOffset get
    Env.putVar name (bpOffset-1) -- -1 because points to one past top of stack.
    return (valAsm ++ pushVal)

-- Calculates int value and reassigns existing value on stack.
assign :: AST.VarName -> AST.DefVal -> St [Instr]
assign name val = Env.tempReg $ \reg -> do
    valAsm <- defVal val reg
    saveAsm <- storeVar name reg
    return (valAsm ++ saveAsm)

-- Return instructions to store variable in register into memory.
storeVar :: AST.VarName -> RegIdx -> St [Instr]
storeVar name reg = do
    bp <- Env.bp
    offset <- Env.getVarOffset name
    return [StoreIdx reg bp offset]

-- Calculates offset into array and int value and reassigns element.
assignArr :: AST.VarName -> AST.IntVal -> AST.IntVal -> St [Instr]
assignArr name idx val =
    Env.tempReg $ \startReg ->
    Env.tempReg $ \idxReg ->
    Env.tempReg $ \valReg -> do
        startAsm <- var name startReg
        idxAsm   <- intVal idx idxReg
        valAsm   <- intVal val valReg
        return (startAsm ++ idxAsm ++ valAsm ++ [StoreBaseIdx valReg startReg idxReg])

-- Returns ASM for conditional execution of a single branch.
ifAsm :: AST.BoolVal -> AST.Stm -> St [Instr]
ifAsm cond body = Env.tempReg $ \condReg -> do
    label   <- Env.freshLabel
    condAsm <- boolVal cond condReg
    let branchAsm = [BF condReg label]
    bodyAsm <- stm body
    return (condAsm ++ branchAsm ++ bodyAsm ++ [Label label])

-- Return ASM for conditional execution of 'then' or 'else' branch.
ifElse :: AST.BoolVal -> AST.Stm -> AST.Stm -> St [Instr]
ifElse cond sThen sElse = Env.tempReg $ \condReg -> do
    elseLabel <- Env.freshLabel
    exitLabel <- Env.freshLabel
    condAsm   <- boolVal cond condReg
    let branchAsm = [BF condReg elseLabel]
    thenAsm <- stm sThen
    let postThenAsm = [B exitLabel, Label elseLabel]
    elseAsm <- stm sElse
    let postElseAsm = [Label exitLabel]
    return (condAsm ++ branchAsm ++ thenAsm ++ postThenAsm ++ elseAsm ++ postElseAsm)

-- Return ASM for while loop.
while :: AST.BoolVal -> AST.Stm -> St [Instr]
while cond body = Env.tempReg $ \condReg -> do
    enterLabel <- Env.freshLabel
    exitLabel  <- Env.freshLabel
    condAsm    <- boolVal cond condReg
    bodyAsm    <- stm body
    return (condAsm ++ [BF condReg exitLabel, Label enterLabel]
         ++ bodyAsm
         ++ condAsm ++ [BT condReg enterLabel, Label exitLabel])

-- Return ASM for performing function call.
call :: AST.FuncCall -> St [Instr]
call = error "call not implemented"

-- Return ASM for performing one statement after another.
comp :: AST.Stm -> AST.Stm -> St [Instr]
comp s1 s2 = do
    s1Asm <- stm s1
    s2Asm <- stm s2
    return (s1Asm ++ s2Asm)

-- Return ASM for printing an int value.
printAsm :: AST.IntVal -> St [Instr]
printAsm val = Env.tempReg $ \reg -> do
    valAsm <- intVal val reg
    return (valAsm ++ [Print reg])

-- Return ASM for printing a newline.
println :: St [Instr]
println = return [PrintLn]

-- Return instructions to store either int value in register, or pointer to array.
defVal :: AST.DefVal -> RegIdx -> St [Instr]
defVal (AST.DefInt val)   reg = intVal val reg
defVal (AST.DefArr elems) reg = arrPtr elems reg

-- Return instructions to push elements of array onto stack and store a pointer
-- to the start of the array in reg.
arrPtr :: [AST.IntVal] -> RegIdx -> St [Instr]
arrPtr elems reg = do
    sp <- Env.sp
    let saveStart = Move reg sp
    pushAsm <- seqPush elems
    return (saveStart:pushAsm)

-- Return instructions to store int value in supplied register.
intVal :: AST.IntVal -> RegIdx -> St [Instr]
intVal (AST.Var name)           = var name
intVal (AST.Lit x)              = lit x
intVal (AST.Result func)        = callResult func
intVal (AST.ArrAccess name idx) = arrAccess name idx
intVal (AST.Add val1 val2)      = op Add intVal val1 val2
intval (AST.Sub val1 val2)      = op Sub intVal val1 val2

-- Return instructions to store bool value in supplied register.
boolVal :: AST.BoolVal -> RegIdx -> St [Instr]
boolVal (AST.TRUE)      = lit 1
boolVal (AST.FALSE)     = lit 0
boolVal (AST.Eq i1 i2)  = op Eq intVal i1 i2
boolVal (AST.NEq i1 i2) = notEq i1 i2
boolVal (AST.Lt i1 i2)  = op Lt intVal i1 i2
boolVal (AST.Gt i1 i2)  = op Lt intVal i2 i1
boolVal (AST.Or b1 b2)  = op Or boolVal b1 b2
boolVal (AST.And b1 b2) = op And boolVal b1 b2

-- Return instructions to load variable/function argument into register.
var :: AST.VarName -> RegIdx -> St [Instr]
var name reg = do
    bp <- Env.bp
    offset <- Env.getVarOffset name
    return [LoadIdx reg bp offset]

-- Return instructions to store an int literal in a register.
lit :: Int -> RegIdx -> St [Instr]
lit x reg = return [MoveI reg (fromIntegral x)]

-- Return instructions to perform function call and store returned value in register.
callResult :: AST.FuncCall -> RegIdx -> St [Instr]
callResult func reg = do
    ret     <- Env.ret
    callAsm <- call func
    let copy = Move reg ret
    return (callAsm ++ [copy])

-- Return instructions to access array element and store value in register.
arrAccess :: AST.VarName -> AST.IntVal -> RegIdx -> St [Instr]
arrAccess name idx reg = Env.tempReg $ \idxReg -> do
    startAsm <- var name reg
    idxAsm <- intVal idx idxReg
    let load = LoadBaseIdx reg reg idxReg
    return (startAsm ++ idxAsm ++ [load])

-- Return instructions to perform an operation using the two values as operands.
-- The result is stored in the supplied register.
op :: BinOp -> (a -> RegIdx -> St [Instr]) -> a -> a -> RegIdx -> St [Instr]
op f convert val1 val2 reg1 = Env.tempReg $ \reg2 -> do
    asm1 <- convert val1 reg1
    asm2 <- convert val2 reg2
    return (asm1 ++ asm2 ++ [f reg1 reg1 reg2])

-- Return instructions for storing whether v1 and v1 are not equal.
notEq :: AST.IntVal -> AST.IntVal -> RegIdx -> St [Instr]
notEq v1 v2 reg = do
    eqAsm <- op Eq intVal v1 v2 reg
    return (eqAsm ++ [Not reg reg])

-- Push the values in the registers to the top of the stack.
-- Assumes sp points to next free address on stack.
--
-- E.g.
-- Before `push [4, 5]`:
--      -----------
--      |    1    |
--      |    4    |
--      |         | <- SP
--      -----------
--
-- After:
--      -----------
--      |    1    |
--      |    4    |
--      |    X    |
--      |    Y    |
--      |         | <- SP
--      -----------
push :: [RegIdx] -> St [Instr]
push regs = do
    sp <- Env.sp
    let store (idx, offset) = StoreIdx idx sp offset
        stores = map store (zip regs [0..])
        incSp  = AddI sp sp (fromIntegral $ length regs)
    Env.incBpOffset (fromIntegral $ length regs)
    return (stores ++ [incSp])

-- Compute each int value individually and push onto stack. Fewer registers than
-- `push` as not all int values are held in registers at the same time. However,
-- requires more sp update instructions.
seqPush :: [AST.IntVal] -> St [Instr]
seqPush vals = Env.tempReg $ \reg -> seqPush' vals reg where
    seqPush' []         _   = return []
    seqPush' (val:vals) reg = do
        valAsm  <- intVal val reg
        pushAsm <- push [reg]
        restAsm <- seqPush' vals reg
        return (valAsm ++ pushAsm ++ restAsm)

-- Pop values from the top of the stack into registers.
-- Assumes sp points to free address on top of stack.
--
-- E.g.
-- Before `pop [4, 5]`
--      -----------
--      |    8    |
--      |    6    |
--      |    1    |
--      |    2    |
--      |         | <- SP
--      -----------
--
-- After:
--       -----------
--       |    8    |
--       |    6    |
--       |    1    | <- SP
--       |    2    |
--       -----------
pop :: [RegIdx] -> St [Instr]
pop regs = do
    sp <- Env.sp
    let load (idx, offset) = LoadIdx idx sp (-offset)
        loads = map load (zip regs [1..])
        decSp = SubI sp sp (fromIntegral $ length regs)
    Env.incBpOffset (-(fromIntegral $ length regs))
    return (loads ++ [decSp])
