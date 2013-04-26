module AnnotatedAbs where

import LexJavalette
import ParJavalette
import AbsJavalette

data AnnotatedProgram = AnnotatedProgram [AnnotatedTopDef]
    deriving (Eq, Ord, Show)

data AnnotatedTopDef = 
    FnDef Type Ident [Arg] AnnotatedBlock
    deriving (Eq, Ord, Show)

data AnnotatedBlock = 
    AnnotatedBlock [AnnotatedStmt]
    deriving (Eq, Ord, Show)

data AnnotatedItem = 
      NoInit Ident
    | Init Ident AnnotatedExp
        deriving (Eq, Ord, Show)

data AnnotatedStmt = 
      Empty
    | BStmt AnnotatedBlock
    | Decl Type [AnnotatedItem]
    | Ass Ident AnnotatedExp
    | Incr Ident
    | Decr Ident
    | Ret AnnotatedExp
    | VRet
    | Cond AnnotatedExp AnnotatedStmt
    | CondElse AnnotatedExp AnnotatedStmt AnnotatedStmt
    | While AnnotatedExp AnnotatedStmt
    | SExp AnnotatedExp
    deriving (Eq, Ord, Show)

data AnnotatedExp = 
   EVar Ident Type
 | ELitInt Integer Type
 | ELitDoub Double Type
 | ELitTrue Type
 | ELitFalse Type
 | EApp Ident [AnnotatedExp] Type
 | EString String Type
 | Neg AnnotatedExp Type
 | Not AnnotatedExp Type
 | EMul AnnotatedExp MulOp AnnotatedExp Type
 | EAdd AnnotatedExp AddOp AnnotatedExp Type
 | ERel AnnotatedExp RelOp AnnotatedExp Type
 | EAnd AnnotatedExp AnnotatedExp Type
 | EOr AnnotatedExp AnnotatedExp Type
  deriving (Eq,Ord,Show)

