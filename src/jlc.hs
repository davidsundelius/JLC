import System.Environment
import System.Exit
import System.IO
import System.Process
import System.FilePath.Posix
import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import AbsJavalette
import LexJavalette
import ParJavalette
import ErrM

import TypeChecker
import Generator

--Compiler
compile :: OutputType -> String -> String -> IO ()
compile target filename s = case pProgram (myLexer s) of
  Bad err  -> do hPutStrLn stderr "ERROR"
                 putStrLn "Syntax error:"
                 putStrLn err
                 exitFailure
  Ok  tree -> case typecheck tree of
                Bad err  -> do hPutStrLn stderr "ERROR"
                               putStrLn "Type error:"
                               putStrLn err
                               exitFailure
                Ok tree' -> do hPutStrLn stderr ("OK")
                               putStrLn "Type checking: OK"
                               generate path classname target tree'
                               assemble target filename
                               where classname = takeBaseName (filename)
                                     dir       = takeDirectory (filename)
                                     path      = dir ++ "/" ++ classname

assemble ::  OutputType -> String -> IO ()
assemble target filename = case target of
  Jasm -> do putStrLn ("Compiling: " ++ path ++ ".j to " ++ path ++ ".class")
             exitCode <- system ("java -jar lib/jasmin.jar " ++ path ++ ".j -d " ++ dir)
             putStrLn (show exitCode)
             putStrLn ("Compilation complete.")
  Lasm -> do putStrLn ("Compiling: " ++ path ++ ".ll to " ++ path ++ ".bc")
             exitCode <- system ("llvm-as " ++ path ++ ".ll -o " ++ path ++ ".bc")
             putStrLn (show exitCode)
             putStrLn ("Linking: " ++ path ++ ".bc with libs to a_out.bc and creating script a.out")
             exitCode <- system ("llvm-ld " ++ path ++ ".bc lib/runtime.bc -o " ++ dir ++ "/a.out")
             putStrLn (show exitCode)
             putStrLn ("Compilation complete.")
  X86  -> do exitCode <- system ("nasm " ++ dir)
             putStrLn (show exitCode)
             putStrLn ("Compilation complete.")
  where classname = takeBaseName (filename)
        dir = takeDirectory (filename)
        path = dir ++ "/" ++ classname

--Input handling
data Flag = OnlyAsm            -- -a
          | TargetL OutputType -- -t
          | Help               -- -h
          deriving (Eq,Show)

flags :: [OptDescr Flag]
flags =
       [Option ['a'] []       (NoArg OnlyAsm)
            "Skips generation of asm and just assembles the file to a class file."
       ,Option ['t'] []       (ReqArg detTarget "jvm/llvm/x86")
            "Tells the compiler to use a specific target language."
       ,Option ['h'] ["help"] (NoArg Help)
            "Print this help message."
       ]

detTarget :: String -> Flag
detTarget s = case s of
  "jvm"  -> TargetL Jasm
  "llvm" -> TargetL Lasm
  "x86"  -> TargetL X86

parse :: [String] -> IO ([Flag],String)
parse argv = case getOpt Permute flags argv of
  (args, fs, []) -> do
    if null fs
    then do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess
    else if Help `elem` args
         then do hPutStrLn stderr (usageInfo header flags)
                 exitWith ExitSuccess
         else return (args, (head fs))
  (_,_,errs)      -> do hPutStrLn stderr (concat errs ++ usageInfo header flags)
                        exitWith (ExitFailure 1)
  where header = "Usage: jlc [-ath] <Source file>"

getTarget :: [Flag] -> OutputType
getTarget []     = Lasm
getTarget (f:fs) = case f of
  TargetL t -> t
  _         -> getTarget fs

main :: IO ()
main =
  do (args, file) <- getArgs >>= parse
     let target = getTarget args
     if OnlyAsm `elem` args then assemble target file else readFile file >>= compile target file
