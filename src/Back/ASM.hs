module Back.ASM where

import qualified Front.AST as AST
import Back.Env (St)
import qualified Back.Env as Env
import Back.Instr
import Control.Monad.State
import Data.List (sortBy)

-- Takes result register and two operand registers.
type BinOp = RegIdx -> RegIdx -> RegIdx -> Instr

prog :: [AST.FuncDef] -> St [Instr]
prog [AST.FuncDef name args _ body] = do
    is <- funcBodyAndStackClean name (map AST.varName args) body
    return ([Label name] ++ is)
prog funcs = fmap concat $ mapM toAsm (mainFirst funcs) where
    toAsm (AST.FuncDef name args _ body) = do
        let argNames = map AST.varName args
        bodyAsm <- funcBodyAndStackClean name argNames body
        return ([Label name] ++ bodyAsm)

-- Sorts function definitions so main is at top of list. Therefore no extra
-- code is required to branch into main.
mainFirst :: [AST.FuncDef] -> [AST.FuncDef]
mainFirst = sortBy order where
    order (AST.FuncDef "main" _ _ _) _ = LT
    order _ (AST.FuncDef "main" _ _ _) = GT
    order _ _ = EQ

-- Outputs saving space for variables, function body, function return, and stack
-- cleanup.
funcBodyAndStackClean :: AST.FuncName -> [AST.VarName] -> AST.Stm -> St [Instr]
funcBodyAndStackClean name args body = do
    let defs  = AST.defs body
        sizes = map (\(n, val) -> (n, AST.size val)) defs
        localVarsSize = AST.totalSize (map snd defs)

    -- Create space for variables declared in function.
    reserveVarSpace <- addConst (fromIntegral localVarsSize)
    asm <- varsAtFrameStart args sizes (funcBody defs name body)
    return (reserveVarSpace ++ asm)

-- Generates ASM for function body and return.
funcBody :: [(AST.VarName, AST.DefVal)] -> AST.FuncName -> AST.Stm -> St [Instr]
funcBody defs name body = do
    assignPtrs <- assignArrPtrs defs
    bodyAsm <- stm body
    endAsm <- funcEnd name body
    return (assignPtrs ++ bodyAsm ++ endAsm)

-- Return instructions to calculate pointers (into callee reserved buffer) and
-- store them in variables.
assignArrPtrs :: [(AST.VarName, AST.DefVal)] -> St [Instr]
assignArrPtrs ns = fmap fst $ foldM f ([], 0) ns where
    f (is, accSize) (name, val) = do
        let accSize' = accSize + AST.size val
        -- -1 because array start is one address after array ptr.
        assignAsm <- assignPtr (accSize' - 1) name val
        return (is ++ assignAsm, accSize')

    assignPtr :: AST.VarSize -> AST.VarName -> AST.DefVal -> St [Instr]
    assignPtr accSize name (AST.DefArr _) = Env.tempReg $ \reg -> do
        sp <- Env.sp
        let getSp = [Move reg sp]
            compPtr = [SubI reg reg (fromIntegral accSize)]
        store <- storeVar name reg
        return (getSp ++ compPtr ++ store)
    assignPtr _ _ _ = return []

-- Return instructions to place at end of function.
-- I.e. SysCall at end of main, return at end of functions (unless they already)
-- have a return.
funcEnd :: AST.FuncName -> AST.Stm -> St [Instr]
funcEnd "main" _ = terminate
funcEnd _ body   | AST.isReturn (AST.lastStm body) = return []
                 | otherwise = ret

stm :: AST.Stm -> St [Instr]
stm (AST.Return val) = retVal val
stm (AST.Def name val) = defVal name val
stm (AST.Assign name val) = defVal name val
stm (AST.AssignArr name arrIdx val) = assignArr name arrIdx val
stm (AST.If cond body) = ifAsm cond body
stm (AST.IfElse cond sThen sElse) = ifElse cond sThen sElse
stm (AST.For name range body) = stm (AST.forAsWhile name range body)
stm (AST.While cond body) = while cond body
stm (AST.Call func) = call func
stm (AST.Comp ss) = comp ss
stm (AST.Print val) = printAsm Print val
stm (AST.PrintC val) = printAsm PrintC val
stm (AST.PrintLn) = println

ret :: St [Instr]
ret = do
    pop <- popLocalVars
    return (pop ++ [Ret])

-- Calculates int value and populates return register with it.
retVal :: AST.IntVal -> St [Instr]
retVal val = do
    reg <- Env.ret
    valAsm <- intVal val reg
    retAsm <- ret
    return (valAsm ++ retAsm)

-- Use SysCall to exit program
terminate :: St [Instr]
terminate = do
    pop <- popLocalVars
    return (pop ++ [SysCall])

-- Return instructions to subtract the size of local vars from stack.
popLocalVars :: St [Instr]
popLocalVars = do
    localVarsSize <- fmap Env.localVarsSize get
    subConst (fromIntegral localVarsSize)

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
call (AST.FuncCall name args) = Env.tempRegs args $ \valsAndRegs -> do
    bp <- Env.bp
    sp <- Env.sp
    lr <- Env.lr
    retLabel <- Env.freshLabel

    let argRegs = map snd valsAndRegs
    evalArgs <- intValAll valsAndRegs

    saveRegs <- push [bp, lr]
    let setRegs = [Move bp sp, MoveLabel lr (LabelAddr retLabel)]
    pushArgs <- push argRegs
    let branch = [B name, Label retLabel]
    popArgs <- subConst (fromIntegral $ length args)
    restoreRegs <- pop [lr, bp]

    return (evalArgs
         ++ saveRegs
         ++ setRegs
         ++ pushArgs
         ++ branch
         ++ popArgs
         ++ restoreRegs)

-- Return ASM for performing one statement after another.
comp :: [AST.Stm] -> St [Instr]
comp [] = return []
comp (s:rest) = do
    sAsm <- stm s
    restAsm <- comp rest
    return (sAsm ++ restAsm)

-- Return ASM for printing an int value.
printAsm :: (RegIdx -> Instr) -> AST.IntVal -> St [Instr]
printAsm p val = Env.tempReg $ \reg -> do
    valAsm <- intVal val reg
    return (valAsm ++ [p reg])

-- Return ASM for printing a newline.
println :: St [Instr]
println = return [PrintLn]

-- Return instructions to store either int value in register, or pointer to array.
defVal :: AST.VarName -> AST.DefVal -> St [Instr]
defVal name (AST.DefInt val)   = defInt name val
defVal name (AST.DefArr elems) = defArr name elems

-- Return instructions to store value at location of variable on stack.
defInt :: AST.VarName -> AST.IntVal -> St [Instr]
defInt name val = Env.tempReg $ \reg -> do
    bp <- Env.bp
    valAsm <- intVal val reg
    offset <- Env.getVarOffset name
    let storeAsm = [StoreIdx { r=reg, base=bp, offset=offset }]
    return (valAsm ++ storeAsm)

-- Return instructions to push elements of array onto stack in location pointed
-- to by value stored in variable.
defArr :: AST.VarName -> [AST.IntVal] -> St [Instr]
defArr name elems = Env.tempReg $ \reg -> do
    varAsm <- var name reg
    storeAsm <- storeAll elems reg
    return (varAsm ++ storeAsm)

-- Return instructions to store int value in supplied register.
intVal :: AST.IntVal -> RegIdx -> St [Instr]
intVal (AST.Var name)           = var name
intVal (AST.Lit x)              = lit x
intVal (AST.Result func)        = callResult func
intVal (AST.ArrAccess name idx) = arrAccess name idx
intVal (AST.Add val1 val2)      = op Add intVal val1 val2
intVal (AST.Sub val1 val2)      = op Sub intVal val1 val2
intVal (AST.Mult val1 val2)     = op Mult intVal val1 val2
intVal (AST.Div val1 val2)      = op Div intVal val1 val2

-- Return instructions to store all int values in supplied registers.
intValAll :: [(AST.IntVal, RegIdx)] -> St [Instr]
intValAll = foldM moveVal [] where
    moveVal instrs (val, reg) = do
        asm <- intVal val reg
        return (instrs ++ asm)

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

-- Compute each int value individually and stores at base + 0, 1, 2, etc
storeAll :: [AST.IntVal] -> RegIdx -> St [Instr]
storeAll vals baseReg = Env.tempReg $ \reg -> storeAll' vals reg 0 where
    storeAll' [] _ _  = return []
    storeAll' (val:vals) reg offset = do
        valAsm  <- intVal val reg
        let storeAsm = [StoreIdx { r=reg, base=baseReg, offset=offset }]
        restAsm <- storeAll' vals reg (offset+1)
        return (valAsm ++ storeAsm ++ restAsm)

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
push [] = return []
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
pop [] = return []
pop regs = do
    sp <- Env.sp
    let load (idx, offset) = LoadIdx idx sp (-offset)
        loads = map load (zip regs [1..])
        decSp = SubI sp sp (fromIntegral $ length regs)
    return (loads ++ [decSp])

-- Returns add instr, or nothing if add 0.
addConst :: Val -> St [Instr]
addConst 0 = return []
addConst n = do
    sp <- Env.sp
    return [AddI sp sp n]

-- Returns sub instr, or nothing if sub 0.
subConst :: Val -> St [Instr]
subConst 0 = return []
subConst n = do
    sp <- Env.sp
    return [SubI sp sp n]

-- Places variables at start of frame, i.e. at bp + 0, 1, 2, etc
varsAtFrameStart :: [AST.VarName] -> [(AST.VarName, AST.VarSize)] -> St [Instr] -> St [Instr]
varsAtFrameStart args localVars st = state $ \sOld ->
    let s  = Env.putArgs args sOld
        s' = Env.putVars localVars (fromIntegral (length args)) s
        (is, sNew) = runState st s'
    in (is, Env.restoreEnv sOld sNew)
