module Generator where

import AbsJavalette

import AbsJasm
import JasmConverter
import PrintJasm

import AbsLasm
import LasmConverter
import PrintLasm
import ErrM

data OutputType = Jasm | Lasm | X86
  deriving (Eq, Show)

generate :: String -> String -> OutputType -> Program -> IO()
generate path name out program = case out of
  Jasm -> do writeFile (file) (printJASM $ cASTJ program name)
             putStrLn ("Generated: " ++ file)
          where file = path ++ ".j"
  Lasm -> do writeFile (file) (printLASM $ cASTL program)
             putStrLn ("Generated: " ++ file)
          where file = path ++ ".ll"
  X86  -> error $ "Not implemented x86 machine code generation yet"
