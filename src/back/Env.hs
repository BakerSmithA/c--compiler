module Back.Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
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
  -- Registers that are not in-use.
  , freeRegs :: Set RegIdx
    -- Where variables are stored on the stack.
  , varAddr :: Map VarName Addr
    -- Current function argument names. Used to tell where to retrieve a variable from.
  , args :: Map VarName ArgIdx
    -- Used to generate fresh labels for branch locations.
  , currLabel :: Int
}

-- Uses ASM environment to compute value.
type St a = State Env a

-- environment with no variables, arguments, or used labels.
empty :: Env
empty = Env 14 15 16 17 (Set.fromList [0..13]) Map.empty Map.empty 0

-- Return index of stack pointer register.
sp :: St RegIdx
sp = fmap spIdx get

-- Return index of link register.
lr :: St RegIdx
lr = fmap lrIdx get

-- Return index of base pointer register.
bp :: St RegIdx
bp = fmap bpIdx get

-- Return index of return value register.
ret :: St RegIdx
ret = fmap retIdx get

-- Returns address associated with variable, or crashes if address not associated
-- with variable.
getVar :: VarName -> St Addr
getVar name = do
    env <- get
    let addr = Map.lookup name (varAddr env)
    return (fromJust addr)

-- Keeps track of a variable and associated stack address.
putVar :: VarName -> Addr -> St ()
putVar name addr = modify $ \env ->
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

-- Removes a register from the free-list.
takeReg :: St RegIdx
takeReg = do
    env <- get
    let (idx, freeRegs') = Set.deleteFindMin (freeRegs env)
    case Set.size freeRegs' of
        0 -> error "No registers available"
        _ -> do
            put (env { freeRegs = freeRegs' })
            return idx

-- Puts a register back on the free-list to be used again.
freeReg :: RegIdx -> St ()
freeReg reg = modify $ \env ->
    env { freeRegs = Set.insert reg (freeRegs env) }

tempReg :: (RegIdx -> St a) -> St a
tempReg f = undefined

runSt :: Env -> St a -> a
runSt env st = fst (runState st env)
