module Generator where

import Control.Monad.State

import AnnotatedAbs as TYP
import AbsJavalette as ABS
import GeneratorContext

type GenState a = StateT GenContext IO a

generation :: AnnotatedProgram -> IO ()
generation = undefined

genTopDef :: AnnotatedTopDef -> GenState ()
genTopDef = undefined

genBlock :: AnnotatedBlock -> GenState ()
genBlock = undefined

genStmt :: AnnotatedStmt -> GenState ()
genStmt TYP.Empty                 = undefined
genStmt (TYP.BStmt block)         = undefined
genStmt (TYP.Decl typeDecl items) = undefined
genStmt (TYP.Ass ident exp)       = undefined
genStmt (TYP.Incr ident)          = undefined
genStmt (TYP.Decr ident)          = undefined
genStmt (TYP.Ret exp)             = undefined
genStmt TYP.VRet                  = undefined
genStmt (TYP.Cond exp stmt)       = undefined
genStmt (TYP.CondElse exp s1 s2)  = undefined


genExp :: AnnotatedExp -> GenState ()
genExp (EVar ident, typeExp)        = undefined
genExp (ELitInt int, typeExp)       = undefined
genExp (ELitDoub double, typeExp)   = undefined
genExp (ELitTrue, typeExp)          = undefined
genExp (ELitFalse, typeExp)         = undefined
genExp (EApp ident exprs, typeExp)  = undefined
genExp (EString string, typeExp)    = undefined
genExp (Neg expr, typeExp)          = undefined
genExp (Not expr, typeExp)          = undefined

genExp (EMul e1 Times e2, typeExp)  = undefined
genExp (EMul e1 Div e2, typeExp)  = undefined
genExp (EMul e1 Mod e2, typeExp)  = undefined

genExp (EAdd e1 Plus e2, typeExp)  = undefined
genExp (EAdd e1 Minus e2, typeExp)  = undefined

genExp (ERel e1 LTH e2, typeExp)  = undefined
genExp (ERel e1 LE e2, typeExp)  = undefined
genExp (ERel e1 GTH e2, typeExp)  = undefined
genExp (ERel e1 GE e2, typeExp)  = undefined
genExp (ERel e1 EQU e2, typeExp)  = undefined
genExp (ERel e1 NE e2, typeExp)  = undefined

genExp (EAnd e1 e2, typeExp)        = undefined
