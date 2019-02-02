module Main where

import Back.ASM
import Back.Instr as Instr
import Back.Env as Env (runSt, empty)
import Front.Parser as Parser
import Data.Word
import qualified Data.ByteString as B
import Text.Megaparsec
import System.Environment

newlines :: Instr -> String
newlines (Instr.Ret) = "\n"
newlines (Instr.SysCall) = "\n"
newlines _           = ""

prettyReg :: RegIdx -> String
prettyReg r = "r" ++ show r

indent :: Instr -> String
indent (Instr.Label _) = ""
indent _               = "    "

showInstrs :: [Instr] -> String
showInstrs is = unlines strNumbered where
    strNumbered = map (\(n, i) -> (show n) ++ "|\t" ++ indent i ++ (pretty prettyReg i) ++ newlines i) numbered
    numbered    = zip [0..] is

writeASM :: [Word8] -> FilePath -> IO ()
writeASM asm path = B.writeFile path (B.pack asm)

compile :: FilePath -> FilePath -> IO ()
compile inPath outPath = do
    contents <- readFile inPath
    case runParser Parser.funcDefs inPath contents of
        Left err -> putStrLn (errorBundlePretty err)
        Right ast -> do
            let instrs = runSt Env.empty (prog ast)
                asm = Instr.asm instrs
            putStrLn (showInstrs instrs)
            writeASM asm outPath

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: c-- <filename.cmm> <out>"
    case args of
        [inPath, outPath] -> compile inPath outPath
        _                 -> error usageMsg
