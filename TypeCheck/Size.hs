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
getLocalaStackSize _ = (0, 0)
{-
getLocalaStackSize (AnnotatedBlock stmts) = getStmts stmts

getStmts :: [AnnotatedStmt] -> (Int, Int)
getStmts [(stm, t):stms] = ((isVariable stm) + fst(size stms), 
                             (isOnStack stm)  + snd(size stms))
  where size = getLocalaStackSize 
        isVariable (Ass _ _) = 1
        isVariable  _       = 0
getStmts [] = (0, 0)

isOnStack :: AnnotatedStmt -> Int
isOnStack (Cond     exp stm) = expOnStack exp + isOnStack stm
isOnStack (CondElse exp stm) = expOnStack exp + isOnStack stm
isOnStack (While    exp stm) = expOnStack exp + isOnStack stm
isOnStack (SExp     exp    ) = expOnStack exp
isOnStack _                = 0

expOnStack :: AnnotatedExp -> Int
expOnStack (EVar      _ _) = 1
expOnStack (ELitInt   _ _) = 1
expOnStack (ELitDoub  _ _) = 1
expOnStack (ELitTrue  _ _) = 1
expOnStack (ELitFalse _ _) = 1
expOnStack  _             = 0
-}

