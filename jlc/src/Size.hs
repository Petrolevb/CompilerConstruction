module Size where


import AnnotatedAbs

{-
   ldc          -> +1
   bipush       -> +1
   const        -> +1
   load         -> +1
   invokestatic -> +1
   store        -> -1
-}

getLocalaStackSize :: AnnotatedBlock -> (Int, Int)
getLocalaStackSize (AnnotatedBlock stmts) = getStmts stmts

getStmts :: [AnnotatedStmt] -> (Int, Int)
getStmts (stm:stms) = ((isVariable stm) + fst(size stms),
                             (isOnStack stm)  + snd(size stms))
  where size = getStmts 
getStmts [] = (1, 0)

isOnStack :: AnnotatedStmt -> Int
isOnStack (BStmt    block  ) = isOnStack' block
isOnStack (Ass      _   _  ) = 1
isOnStack (Cond     exp stm) = expOnStack exp + isOnStack stm
isOnStack (CondElse exp stm1 stm2) = expOnStack exp
                                   + isOnStack stm1
                                   + isOnStack stm2
isOnStack (While    exp stm) = expOnStack exp + isOnStack stm
isOnStack (SExp     exp    ) = expOnStack exp
isOnStack _                = 0

isOnStack' (AnnotatedBlock []    ) = 0
isOnStack' (AnnotatedBlock (b:bs)) = isOnStack b + isOnStack' (AnnotatedBlock bs)

expOnStack :: AnnotatedExp -> Int
expOnStack (EVar       _ _) = 1
expOnStack (ELitInt    _ _) = 1
expOnStack (ELitDoub   _ _) = 1
expOnStack (EString    _ _) = 1
expOnStack (ELitTrue   _  ) = 1
expOnStack (ELitFalse  _  ) = 1
expOnStack (Neg        e _) = expOnStack e
expOnStack (Not        e _) = expOnStack e
expOnStack (EAdd e1 _ e2 _) = 1 + expOnStack e1 + expOnStack e2
expOnStack (EMul e1 _ e2 _) = 1 + expOnStack e1 + expOnStack e2
expOnStack  _              = 0

isVariable :: AnnotatedStmt -> Int
isVariable (Decl _ item) = length item
isVariable  _        = 0
