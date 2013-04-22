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

data AnnotatedStmt = 
      Empty
    | BStmt AnnotatedBlock
    | Decl Type [Item]
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

type AnnotatedExp = (Expr, Type) 
