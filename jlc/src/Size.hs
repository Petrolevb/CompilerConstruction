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
getStmts [] = (1, 1)

isOnStack :: AnnotatedStmt -> Int
isOnStack (BStmt    (AnnotatedBlock block)) = sum $ map isOnStack block
isOnStack (Ass      _   _  ) = 1
isOnStack (Cond     exp stm) = expOnStack exp + isOnStack stm
isOnStack (CondElse exp stm1 stm2) = expOnStack exp
                                   + isOnStack stm1
                                   + isOnStack stm2
isOnStack (While    exp stm) = expOnStack exp + isOnStack stm
isOnStack (SExp     exp    ) = expOnStack exp
isOnStack _                = 0

expOnStack :: AnnotatedExp -> Int
expOnStack (EVar       _ _) = 1
expOnStack (ELitInt    _ _) = 1
expOnStack (ELitDoub   _ _) = 1
expOnStack (ELitTrue   _  ) = 1
expOnStack (ELitFalse  _  ) = 1
expOnStack (EApp _  _    _) = 1
expOnStack (EString    _ _) = 1
expOnStack (Neg        e _) = expOnStack e
expOnStack (Not        e _) = expOnStack e
expOnStack (EAdd e1 _ e2 _) = 1 + expOnStack e1 + expOnStack e2
expOnStack (EMul e1 _ e2 _) = 1 + expOnStack e1 + expOnStack e2
expOnStack (ERel e1 _ e2 _) = expOnStack e1 + expOnStack e2
expOnStack (EAnd e1 e2   _) = expOnStack e1 + expOnStack e2
expOnStack (EOr  e1 e2   _) = expOnStack e1 + expOnStack e2

isVariable :: AnnotatedStmt -> Int
isVariable (BStmt (AnnotatedBlock block)) = sum $ map isVariable block
isVariable (Decl     _ item ) = length item
isVariable (Ass      _ _    ) = 1
isVariable (Cond     _ stmt ) = isVariable stmt
isVariable (CondElse _ s1 s2) = isVariable s1 + isVariable s2
isVariable (While    _ stmt ) = isVariable stmt
isVariable  _                 = 0

