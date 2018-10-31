module Back.ASM where

import qualified Front.AST as AST
import Back.Env
import Back.Instr

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
ret = undefined

-- Calculates int value and stores on stack.
def :: AST.VarName -> AST.DefVal -> St [Instr]
def = undefined

-- Calculates int value and reassigns existing value on stack.
assign :: AST.VarName -> AST.DefVal -> St [Instr]
assign = undefined

-- Calculates offset into array and int value and reassigns element.
assignArr :: AST.VarName -> AST.IntVal -> AST.IntVal -> St [Instr]
assignArr = undefined

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
comp = undefined

-- Return ASM for printing an int value.
printAsm :: AST.IntVal -> St [Instr]
printAsm = undefined

-- Return ASM for printing a newline.
println :: St [Instr]
println = undefined
