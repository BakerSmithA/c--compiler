module Main where

import Back.ASM
import Back.Instr as Instr
import Back.Env (Env)
import Back.Env as Env
import Front.Parser as Parser
import Data.Word
import qualified Data.ByteString as B
import Text.Megaparsec
import System.Environment
import Text.Printf

newlines :: Instr -> String
newlines (Instr.Ret) = "\n"
newlines (Instr.SysCall) = "\n"
newlines _           = ""

-- Prints general purpose registers as 'rX', where X is the number of the register.
-- Prints special purpose registers using their abbreviation, e.g. Stack Pointer is 'sp'.
prettyReg :: Env -> RegIdx -> String
prettyReg env r | Env.spIdx env  == r = "sp"
                | Env.lrIdx env  == r = "lr"
                | Env.bpIdx env  == r = "bp"
                | Env.retIdx env == r = "ret"
                | otherwise           = "r" ++ show r

-- Used to indent instructions that are not labels.
-- Makes easier to segment different pieces of the program.
indent :: Instr -> String
indent (Instr.Label _) = ""
indent _               = "    "

showInstrs :: Env -> [Instr] -> String
showInstrs env is = unlines strNumbered where
    strNumbered = map (\(n, i) -> (numStr n) ++ "| " ++ indent i ++ (prettyInstr i) ++ newlines i) numbered
    numbered    = zip [0..] is
    prettyInstr i = pretty (prettyReg env) i
    -- Used to pad line numbers according to the maximum line number index.
    numStr :: Int -> String
    numStr n    = printf ("%0" ++ show (numDigits (length is)) ++ "d") n

numDigits :: Int -> Integer
numDigits n = toInteger (round (logBase 10 (fromIntegral n)))

writeASM :: [Word8] -> FilePath -> IO ()
writeASM asm path = B.writeFile path (B.pack asm)

compile :: FilePath -> FilePath -> IO ()
compile inPath outPath = do
    contents <- readFile inPath
    case runParser Parser.funcDefs inPath contents of
        Left err -> putStrLn (errorBundlePretty err)
        Right ast -> do
            let env    = Env.empty
                instrs = runSt env (prog ast)
                asm    = Instr.asm instrs
            putStrLn (showInstrs env instrs)
            writeASM asm outPath

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: c-- <filename.cmm> <out>"
    case args of
        [inPath, outPath] -> compile inPath outPath
        _                 -> error usageMsg
