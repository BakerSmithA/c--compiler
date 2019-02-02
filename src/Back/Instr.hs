module Back.Instr where

import Data.Int (Int32)
import Data.Word (Word32, Word8)
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (foldl, foldr, length, reverse, zip)
import Data.Map (Map)
import qualified Data.Map as Map

type RegIdx = Word8
type Addr = Word32
type Val = Int32
type Label = String

data LabelAddr
    = EndAddr         -- Address of ending instruction, so no more instructions will be executed.
    | LabelAddr Label -- Address of label.
    deriving (Eq, Show)

data AddrData = AddrData {
    labelAddrs :: Map Label Addr
  , endAddr    :: Addr
}

addr :: Label -> AddrData -> Addr
addr label (AddrData m _) =
    case Map.lookup label m of
        Nothing   -> error ("Label " ++ (show label) ++ "does not exist")
        Just addr -> addr - 1 -- -1 because PC is incremented by 1 every cycle.

labelAddr :: LabelAddr -> AddrData -> Addr
labelAddr (EndAddr)         addrData = endAddr addrData
labelAddr (LabelAddr label) addrData = addr label addrData

-- Finds the labels in the instruction and generates a mapping from labels to
-- byte addresses in the instructions.
findLabelAddrs :: [Instr] -> Map Label Addr
findLabelAddrs instrs = fst (foldl f (Map.empty, 0) instrs) where
    f (m, addr) instr = (labelled instr addr m, addr + size instr)

    labelled (Label l) addr m = Map.insert l addr m
    labelled _ _ m = m

-- Finds end address, i.e. address just past the last instruction.
findEndAddr :: [Instr] -> Addr
findEndAddr is = (foldr ((+) . size) 0 is) - 2 -- -1 for last index, -1 for offset due to PC update.

data Instr
    -- Memory
    = MoveI        { r :: RegIdx, val  :: Val }                       -- r <- val
    | MoveLabel    RegIdx LabelAddr                                    -- r <- *label
    | Move         { r :: RegIdx, from :: RegIdx }                    -- r <- [from]
    | LoadIdx      { r :: RegIdx, base :: RegIdx, offset  :: Val }   -- r <- [[base] + offset]
    | LoadBaseIdx  { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r <- [[base] + [R_offset]]
    | StoreIdx     { r :: RegIdx, base :: RegIdx, offset  :: Val }   -- r -> [[base] + offset]
    | StoreBaseIdx { r :: RegIdx, base :: RegIdx, rOffset :: RegIdx } -- r -> [[base] + [R_offset]]
    -- Arithmetic/Logic
    | Add  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] + [y]
    | AddI { r :: RegIdx, x :: RegIdx, i :: Val }    -- r <- [x] + i
    | Sub  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] - [y]
    | SubI { r :: RegIdx, x :: RegIdx, i :: Val }    -- r <- [x] - i
    | Mult { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] * [y]
    | Div  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] / [y]
    | Eq   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] == [y]
    | Lt   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] < [y]
    | Or   { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] || [y]
    | And  { r :: RegIdx, x :: RegIdx, y :: RegIdx } -- r <- [x] && [y]
    | Not  { r :: RegIdx, x :: RegIdx }              -- r <- ![x]
    -- Branching
    | B  { label :: Label }              -- Unconditional branch to label.
    | BT { r :: RegIdx, label :: Label } -- Branch to label if r == 1
    | BF { r :: RegIdx, label :: Label } -- Branch to label if r == 0
    | Ret                                -- Branch to address in link register.
    | SysCall                            -- Terminates execution.
    -- Debugging
    | Print  { r :: RegIdx } -- Print value in a register.
    | PrintC { r :: RegIdx } -- Print value in a register as an ASCII character.
    | PrintLn                -- Print a newline.
    -- Extra
    | NoOp
    | Label Label  -- Used to label branch locations.
    deriving (Eq, Show)

prettyLabel :: LabelAddr -> String
prettyLabel (EndAddr)         = ".end"
prettyLabel (LabelAddr label) = "." ++ label

pretty :: (RegIdx -> String) -> Instr -> String
pretty sr (MoveI r val)          = "MOVI " ++ sr r ++ " #" ++ show val
pretty sr (MoveLabel r label)    = "MOVI " ++ sr r ++ " " ++ prettyLabel label
pretty sr (Move r src)           = "MOV "  ++ sr r ++ " " ++ sr src
pretty sr (LoadIdx r b off)      = "LD "   ++ sr r ++ " " ++ sr b ++ " #" ++ show off
pretty sr (LoadBaseIdx r b off)  = "LD "   ++ sr r ++ " " ++ sr b ++ " " ++ sr off
pretty sr (StoreIdx r b off)     = "ST "   ++ sr r ++ " " ++ sr b ++ " #" ++ show off
pretty sr (StoreBaseIdx r b off) = "ST "   ++ sr r ++ " " ++ sr b ++ " " ++ sr off
pretty sr (Add r x y)            = "ADD "  ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (AddI r x y)           = "ADDI " ++ sr r ++ " " ++ sr x ++ " #" ++ show y
pretty sr (Sub r x y)            = "SUB "  ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (SubI r x y)           = "SUBI " ++ sr r ++ " " ++ sr x ++ " #" ++ show y
pretty sr (Mult r x y)           = "MUL "  ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (Div r x y)            = "DIV "  ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (Eq r x y)             = "EQ "   ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (Lt r x y)             = "LT "   ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (Or r x y)             = "OR "   ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (And r x y)            = "AND "  ++ sr r ++ " " ++ sr x ++ " " ++ sr y
pretty sr (Not r x)              = "NOT "  ++ sr r ++ " " ++ sr x
pretty _  (B label)              = "B ."   ++ label
pretty sr (BT r label)           = "BT "  ++ sr r ++ " ." ++ label
pretty sr (BF r label)           = "BF "  ++ sr r ++ " ." ++ label
pretty _  (Ret)                  = "RET"
pretty _  (SysCall)              = "EXIT"
pretty sr (Print r)              = "PRINT " ++ sr r
pretty sr (PrintC r)             = "PRINTC " ++ sr r
pretty _  (PrintLn)              = "PRINTLN"
pretty _  (NoOp)                 = "NOP"
pretty _  (Label label)          = "." ++ label ++ ":"

encodeW32 :: Word32 -> [Word8]
encodeW32 = reverse . unpack . toLazyByteString . word32LE

encodeI32 :: Int32 -> [Word8]
encodeI32 = reverse . unpack . toLazyByteString . int32LE

-- Returns size of an instruction in the outputted bytecode. 0 for instructions
-- that are not outputted, e.g. labels. 1 otherwise.
size :: Instr -> Addr
size (NoOp) = 0
size (Label _) = 0
size _ = 1

-- Returns instruction encoded in bytes.
encoded :: Instr -> AddrData -> [Word8]
-- Memory
encoded (MoveI r v)                   _ = [0, r] ++ encodeI32 v
encoded (MoveLabel r label)           d = [0, r] ++ encodeW32 (labelAddr label d)
encoded (Move r from)                 _ = [14, r, from]
encoded (LoadIdx r base offset)       _ = [1, r, base] ++ encodeI32 offset
encoded (LoadBaseIdx r base rOffset)  _ = [2, r, base, rOffset]
encoded (StoreIdx r base offset)      _ = [3, r, base] ++ encodeI32 offset
encoded (StoreBaseIdx r base rOffset) _ = [4, r, base, rOffset]
-- Arithmetic/Logic
encoded (Add r x y)  _ = [5, r, x, y]
encoded (AddI r x i) _ = [16, r, x] ++ encodeI32 i
encoded (Sub r x y)  _ = [6, r, x, y]
encoded (SubI r x i) _ = [17, r, x] ++ encodeI32 i
encoded (Mult r x y) _ = [18, r, x, y]
encoded (Div r x y)  _ = [23, r, x, y]
encoded (Eq r x y)   _ = [7, r, x, y]
encoded (Lt r x y)   _ = [19, r, x, y]
encoded (Or r x y)   _ = [8, r, x, y]
encoded (And r x y)  _ = [9, r, x, y]
encoded (Not r x)    _ = [15, r, x]
-- Branching
encoded (B label)    d = [10]    ++ encodeW32 (addr label d)
encoded (BT r label) d = [11, r] ++ encodeW32 (addr label d)
encoded (BF r label) d = [22, r] ++ encodeW32 (addr label d)
encoded (Ret)        _ = [12]
encoded (SysCall)    _ = [21]
-- Debugging
encoded (Print r)  _ = [13, r]
encoded (PrintC r) _ = [24, r]
encoded (PrintLn)  _ = [20]
-- Extra
encoded (NoOp)    _ = []
encoded (Label _) _ = []

allEncoded :: [Instr] -> AddrData -> [Word8]
allEncoded instrs addrs = foldl f [] instrs where
    f instrs instr = instrs ++ encoded instr addrs

asm :: [Instr] -> [Word8]
asm instrs =
    let labels   = findLabelAddrs instrs
        end      = findEndAddr instrs
        addrData = AddrData labels end
    in allEncoded instrs addrData
