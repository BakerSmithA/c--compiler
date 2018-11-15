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
prog [AST.FuncDef name _ _ body] = do
    bodyAsm <- funcBodyAndStackClean name [] body
    return (bodyAsm ++ [SysCall])
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
    sp <- Env.sp

    let defs = AST.defs body
        size = AST.size (map snd defs)

    -- Create space for variables declared in function.
    reserveVarSpace <- addConst (fromIntegral size)
    asm <- varsAtFrameStart (args ++ map fst defs) size (funcBody name body)
    return (reserveVarSpace ++ asm)

-- Generates ASM for function body and return.
funcBody :: AST.FuncName -> AST.Stm -> St [Instr]
funcBody name body = do
    bodyAsm <- stm body
    endAsm <- funcEnd name body
    return (bodyAsm ++ endAsm)

-- Return instructions to place at end of function.
-- I.e. SysCall at end of main, return at end of functions (unless they already)
-- have a return.
funcEnd :: AST.FuncName -> AST.Stm -> St [Instr]
funcEnd "main" _ = return [SysCall]
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
stm (AST.While cond body)= while cond body
stm (AST.Call func) = call func
stm (AST.Comp ss) = comp ss
stm (AST.Print val) = printAsm val
stm (AST.PrintLn) = println

ret :: St [Instr]
ret = do
    sp <- Env.sp
    localVarsSize <- fmap Env.localVarsSize get
    pop <- subConst (fromIntegral localVarsSize)
    return (pop ++ [Ret])

-- Calculates int value and populates return register with it.
retVal :: AST.IntVal -> St [Instr]
retVal val = do
    reg <- Env.ret
    valAsm <- intVal val reg
    retAsm <- ret
    return (valAsm ++ retAsm)

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
printAsm :: AST.IntVal -> St [Instr]
printAsm val = Env.tempReg $ \reg -> do
    valAsm <- intVal val reg
    return (valAsm ++ [Print reg])

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
    bp <- Env.bp
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
varsAtFrameStart :: [AST.VarName] -> Int -> St [Instr] -> St [Instr]
varsAtFrameStart names localVarsSize st = state $ \sOld ->
    let (is, sNew) = runState st (Env.putVarsAtStart names localVarsSize sOld)
    in (is, Env.restoreEnv sOld sNew)
--
-- -- Compute ASM with zeroed Bp, then return Bp to value before call.
-- zeroedBp :: St [Instr] -> St [Instr]
-- zeroedBp st = state $ \sOld ->
--     let savedBpOffset = Env.bpOffset sOld
--         (is, sNew) = runState st (Env.setBpOffset 0 sOld)
--     in (is, Env.setBpOffset savedBpOffset sNew)
--
-- -- Restores free registers, mapping from variables to addresses, and bpOffset
-- -- after block.
-- block :: St [Instr] -> St [Instr]
-- block = undefined

-- block st = state $ \sOld ->
--     let (is, sNew) = runState st sOld
--         sRestored  = Env.restoreEnv sOld sNew
--         bpOld      = Env.bpOffset sOld
--         bpNew      = Env.bpOffset sNew
--         sp         = Env.spIdx sNew
--         decSp      = SubI sp sp (bpNew - bpOld)
--     in if bpNew - bpOld == 0
--         then (is, sRestored)
--         else (is ++ [decSp], sRestored) -- Only dec sp if it was modified
