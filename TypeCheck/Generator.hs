module Generator where

import Control.Monad.State

import AnnotatedAbs
import GeneratorContext

type GenState a = StateT GenContext IO a

generation :: AnnotatedProgram -> IO ()
generation = undefined

genTopDef :: AnnotatedTopDef -> IO ()
genTopDef = undefined

genBlock :: AnnotatedBlock -> IO ()
genBlock = undefined

genStmt :: AnnotatedStmt -> IO ()
genStmt = undefined

genExp :: AnnotatedExp -> IO ()
genExp = undefined
