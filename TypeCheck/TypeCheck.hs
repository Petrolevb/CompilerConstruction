-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import LexJavalette
import ParJavalette
import SkelJavalette
import PrintJavalette
import AbsJavalette

import AnnotatedAbs
import TypeChecker

import Generator

import ErrM

type ParseFun a = [Token] -> Err a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = 
    case p (myLexer s) of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn s
           Ok  tree -> case typecheck tree of
                            Bad s  -> do
                                ioError (userError "ERROR")
                                putStrV v $ "\nFail to anotate : " ++ s 
                            Ok at  -> do 
                                generation at
                                ioError (userError "OK")

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProgram
            "-s":fs -> mapM_ (runFile 0 pProgram) fs
            "-b":"JVM":fs  -> mapM_ (runFile 2 pProgram) fs 
            "-b":"LLVM":fs -> fail "LLVM not implemented yet"
            "-b":"x86":fs  -> fail "x86 not implemented yet"
            fs -> mapM_ (runFile 2 pProgram) fs





