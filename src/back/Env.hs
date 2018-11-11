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
    -- Offset of where variables/arguments are stored on the stack, relative to
    -- base pointer (bp).
  , varBpOffset :: Map VarName Val
    -- Current offset past base pointer. Works like stack pointer, but during
    -- compile time used to keep track of where variables are located on stack.
  , bpOffset :: Val
    -- Used to generate fresh labels for branch locations.
  , currLabel :: Int
}

-- Uses ASM environment to compute value.
type St a = State Env a

-- Environment with no variables, arguments, or used labels.
empty :: Env
empty = Env 13 14 15 16 (Set.fromList [0..12]) Map.empty 0 0

-- Environment containing variables at given offset past bp.
fromVars :: [(VarName, Val)] -> Env
fromVars = foldr f empty where
    f :: (VarName, Val) -> Env -> Env
    f (name, offset) env = env { varBpOffset = Map.insert name offset (varBpOffset env) }

restoreEnv :: Env -> Env -> Env
restoreEnv old new = new {
    freeRegs = (freeRegs old)
  , varBpOffset = (varBpOffset old)
  , bpOffset = (bpOffset old)
}

-- Sets the BpOffset to 0.
setBpOffset :: Val -> Env -> Env
setBpOffset offset env = env { bpOffset = offset }

-- Adds arguments to 0.. bpOffset, i.e. at top of call frame.
-- Also increments bpOffset.
putArgs :: [VarName] -> Env -> Env
putArgs names env = env { varBpOffset = varBpOffset', bpOffset = bpOffset' } where
    bpOffset'    = (bpOffset env) + (fromIntegral $ length names)
    varBpOffset' = foldr ins (varBpOffset env) (zip names [0..])
    ins (name, reg) = Map.insert name reg

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

-- Returns whether a variable exists in the environment.
varExists :: VarName -> St Bool
varExists name = do
    env <- get
    return (Map.member name (varBpOffset env))

-- Returns address associated with variable, or crashes if address not associated
-- with variable.
getVarOffset :: VarName -> St Val
getVarOffset name = do
    env <- get
    let addr = Map.lookup name (varBpOffset env)
    return (fromJust addr)

-- Keeps track of a variable and associated stack address. Stack address is
-- calculated as an offset past the base pointer, the offset is returned.
putVar :: VarName -> Val -> St ()
putVar name offset = modify $ \env ->
    env { varBpOffset = Map.insert name offset (varBpOffset env) }

-- Returns a fresh label to use as a branch location.
freshLabel :: St String
freshLabel = do
    env <- get
    put (env { currLabel = (currLabel env) + 1 })
    return (show (currLabel env))

takeEnvReg :: Env -> (RegIdx, Env)
takeEnvReg env =
    let (idx, freeRegs') = Set.deleteFindMin (freeRegs env)
    in case Set.size freeRegs' of
        0 -> error "No registers available"
        _ -> (idx, env { freeRegs = freeRegs' })

-- Removes a register from the free-list.
takeReg :: St RegIdx
takeReg = do
    env <- get
    let (reg, env') = takeEnvReg env
    put env'
    return reg

-- Puts a register back on the free-list to be used again.
freeReg :: RegIdx -> St ()
freeReg reg = modify $ \env ->
    env { freeRegs = Set.insert reg (freeRegs env) }

-- Take a register for the duration of the supplied function, and put back after.
tempReg :: (RegIdx -> St a) -> St a
tempReg f = do
    reg <- takeReg
    res <- f reg
    freeReg reg
    return res

-- Takes enough registers to give to each elements of xs. All registers are
-- given back after f finishes.
tempRegs :: [a] -> ([(a, RegIdx)] -> St b) -> St b
tempRegs xs f = do
    valsAndRegs <- mapM (\x -> fmap (\reg -> (x, reg)) takeReg) xs
    res <- f valsAndRegs
    mapM freeReg (fmap snd valsAndRegs)
    return res

-- Increment offset past base pointer. Used to keep track of where variables are
-- stored on the stack.
incBpOffset :: Val -> St ()
incBpOffset delta = modify $ \env ->
    env { bpOffset = (bpOffset env) + delta }

runSt :: Env -> St a -> a
runSt env st = fst (runState st env)
