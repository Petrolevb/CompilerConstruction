module TypeChecker where 

import LexJavalette
import ParJavalette
import AbsJavalette

import ErrM
import Control.Monad.State

import AnotatedAbs

import Context
import BuildEnv

type ErrTypeCheck a = StateT Env Err a


createBaseEnv :: Program -> Env
createBaseEnv = undefined

typecheck :: Program -> Err AnotatedProgram
typecheck program = evalStateT (anotateCheckProg program) (createBaseEnv program)



anotateCheckProg :: Program -> ErrTypeCheck AnotatedProgram
anotateCheckProg = undefined

typedefs :: TopDef -> ErrTypeCheck AnotatedTopDef
typedefs = undefined


















