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

-- Uses ASM environment to compute value.
type St a = State Env a

-- Keeps track of a variable and associated stack address.
addVar :: VarName -> Addr -> St ()
addVar name addr = modify $ \env ->
    env { varAddr = Map.insert name addr (varAddr env) }

-- Sets the arguments of the function ASM is currently being generated for.
setArgs :: [VarName] -> St ()
setArgs args = modify $ \env ->
    env { args = Map.fromList (zip args [0..]) }

-- Returns a fresh label to use as a branch location.
freshLabel :: St String
freshLabel = do
    env <- get
    put (env { currLabel = (currLabel env) + 1 })
    return (show (currLabel env))
