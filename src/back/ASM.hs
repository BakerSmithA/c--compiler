module Back.ASM where

import qualified Front.AST as AST
import Back.Env (St)
import qualified Back.Env as Env
import Back.Instr

-- Takes result register and two operand registers.
type BinOp = RegIdx -> RegIdx -> RegIdx -> Instr

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
def name val = do
    bpOffset <- Env.putVar name
    storeVar bpOffset val

-- Calculates int value and reassigns existing value on stack.
assign :: AST.VarName -> AST.DefVal -> St [Instr]
assign name val = do
    bpOffset <- Env.getVarOffset name
    storeVar bpOffset val

-- Calculates int value and stores value on stack at given offset from bp.
storeVar :: Val -> AST.DefVal -> St [Instr]
storeVar bpOffset val = Env.tempReg $ \reg -> do
    bp <- Env.bp
    valAsm <- defVal val reg
    return (valAsm ++ [StoreIdx reg bp bpOffset])

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
ifAsm = undefined

-- Return ASM for conditional execution of 'then' or 'else' branch.
ifElse :: AST.BoolVal -> AST.Stm -> AST.Stm -> St [Instr]
ifElse = undefined

-- Return ASM for while loop.
while :: AST.BoolVal -> AST.Stm -> St [Instr]
while = undefined

-- Return ASM for performing function call.
call :: AST.FuncCall -> St [Instr]
call = undefined

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
defVal (AST.DefArr elems) reg = error "cannot define array"

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
    return (stores ++ [incSp])

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
    return (loads ++ [decSp])
