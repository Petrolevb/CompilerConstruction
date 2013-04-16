module AnotatedAbs where

import LexJavalette
import ParJavalette
import AbsJavalette

data AnotatedProgram = AnotatedProgram [AnotatedTopDef]
    deriving (Eq, Ord, Show)

data AnotatedTopDef = 
    FnDef Type Ident [Arg] AnotatedBlock
    deriving (Eq, Ord, Show)

data AnotatedBlock = 
    AnotatedBlock [AnotatedStmt]
    deriving (Eq, Ord, Show)

data AnotatedStmt = 
      Empty
    | BStmt AnotatedBlock
    | Decl Type [Item]
    | Ass Ident AnotatedExp
    | Incr Ident
    | Decr Ident
    | Ret AnotatedExp
    | VRet
    | Cond AnotatedExp AnotatedStmt
    | CondElse AnotatedExp AnotatedStmt AnotatedStmt
    | While AnotatedExp AnotatedStmt
    | SExp AnotatedExp
    deriving (Eq, Ord, Show)

type AnotatedExp = (Expr, Type) 
