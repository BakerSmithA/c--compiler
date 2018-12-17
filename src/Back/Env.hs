module Back.Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
    -- Total size of variables declared in the current function. Used to decrement
    -- the SP at function return. This DOES NOT include size of arguments.
  , localVarsSize :: Int
    -- Used to generate fresh labels for branch locations.
  , currLabel :: Int
}

-- Uses ASM environment to compute value.
type St a = State Env a

-- Environment with no variables, arguments, or used labels.
empty :: Env
empty = Env 12 13 14 15 (Set.fromList gpr) Map.empty 0 0 where
    -- General Purpose Registers, does not include 11 as reserved for PC.
    gpr = [0..10]

-- Environment containing variables at given offset past bp.
fromVars :: [(VarName, Val)] -> Env
fromVars = foldr f empty where
    f :: (VarName, Val) -> Env -> Env
    f (name, offset) env = env { varBpOffset = Map.insert name offset (varBpOffset env) }

restoreEnv :: Env -> Env -> Env
restoreEnv old new = new {
    freeRegs = (freeRegs old)
  , varBpOffset = (varBpOffset old)
  , localVarsSize = (localVarsSize old)
}

-- Adds arguments to bp + 0, 1, 2, etc, i.e. at top of call frame.
putArgs :: [VarName] -> Env -> Env
putArgs names env = env { varBpOffset = varBpOffset' } where
    varBpOffset' = foldr ins (varBpOffset env) (zip names [0..])
    ins (name, reg) = Map.insert name reg

-- Add vars to bp at offset according to accumulated size of previous variables.
putVars :: [(VarName, VarSize)] -> Addr -> Env -> Env
putVars ns offset env = env { varBpOffset = varBpOffset', localVarsSize = fromIntegral totalSize } where
    (varBpOffset', totalSize) = foldr ins (varBpOffset env, 0) ns
    ins (name, size) (vars, accSize) = (Map.insert name (fromIntegral (accSize + offset')) vars, accSize + size)
    offset' = fromIntegral offset

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
getVarOffset :: VarName -> St Val
getVarOffset name = do
    env <- get
    case Map.lookup name (varBpOffset env) of
        Nothing -> error ("No variable with name: " ++ name)
        Just val -> return val

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
    _ <- mapM freeReg (fmap snd valsAndRegs)
    return res

runSt :: Env -> St a -> a
runSt env st = fst (runState st env)
