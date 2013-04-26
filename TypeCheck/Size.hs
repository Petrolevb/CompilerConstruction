module Size where


import AnnotatedAbs

{-
   ldc    -> +1
   bipush -> +1
   const  -> +1
   load   -> +1
   store  -> -1
-}

getLocalaStackSize :: AnnotatedBlock -> (Int, Int)
getLocalaStackSize (AnnotatedBlock stmts) = getStmts stmts

getStmts :: [AnnotatedStmt] -> (Int, Int)
getStmts (stm:stms) = ((isVariable stm) + fst(size stms),
                             (isOnStack stm)  + snd(size stms))
{- getStmts [((Ass e t), _):stms] = ((isVariable (Ass e t)) + fst(size stms),
                             (isOnStack (Ass e t))  + snd(size stms)) -}
  where size = getStmts 
getStmts [] = (0, 0)

isOnStack :: AnnotatedStmt -> Int
isOnStack (Cond     exp stm) = expOnStack exp + isOnStack stm
isOnStack (CondElse exp stm1 stm2) = expOnStack exp
                                   + isOnStack stm1
                                   + isOnStack stm2
isOnStack (While    exp stm) = expOnStack exp + isOnStack stm
isOnStack (SExp     exp    ) = expOnStack exp
isOnStack _                = 0

expOnStack :: AnnotatedExp -> Int
expOnStack (EVar      _ _) = 1
expOnStack (ELitInt   _ _) = 1
expOnStack (ELitDoub  _ _) = 1
expOnStack (ELitTrue  _  ) = 1
expOnStack (ELitFalse _  ) = 1
expOnStack  _              = 0

isVariable :: AnnotatedStmt -> Int
isVariable (Ass _ _) = 1
isVariable  _        = 0
