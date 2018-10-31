module Back.Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Front.AST
import Back.Instr

-- Index of argument in list of arguments,
type ArgIdx = Int

data Env = Env {
    -- Index of Stack Pointer (SP).
    spIdx :: RegIdx
    -- Index of Link Register (LR).
  , lrIdx :: RegIdx
    -- Index of Base Pointer (BP).
  , bpIdx :: RegIdx
    -- Index of return value register (EAX in x86)
  , retIdx :: RegIdx
    -- Where variables are stored on the stack.
  , varAddr :: Map VarName Addr
    -- Current function argument names. Used to tell where to retrieve a variable from.
  , args :: Map VarName ArgIdx
    -- Used to generate fresh labels for branch locations.
  , currLabel :: Int
}
