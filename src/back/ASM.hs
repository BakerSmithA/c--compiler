module Back.ASM where

import qualified Front.AST as AST
import Back.Env
import Back.Instr

stm :: AST.Stm -> St [Instr]
stm (AST.Return val) = undefined
stm (AST.Def name val) = undefined
stm (AST.Assign name val) = undefined
stm (AST.AssignArr name arrIdx val) = undefined
stm (AST.If cond body) = undefined
stm (AST.IfElse cond sThen sElse) = undefined
stm (AST.For name range body) = undefined
stm (AST.While cond body)= undefined
stm (AST.Call func) = undefined
stm (AST.Comp s1 s2) = undefined
stm (AST.NoOp) = undefined
stm (AST.Print val) = undefined
stm (AST.PrintLn) = undefined
