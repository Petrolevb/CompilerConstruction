module Size where


import AnnotatedAbs as TYP
import AbsJavalette as ABS

{-
   ldc    -> +1
   bipush -> +1
   const  -> +1
   load   -> +1
   store  -> -1
-}

getLocalaStackSize :: AnnotatedBlock -> (Int, Int)
getLocalaStackSize [(stm, t):stms] = ((isVariable stm) + fst(size),
                                      (isOnStack stm)  + snd(size))
getLocalaStackSize [] = (0, 0)
  where size = getLocalaStackSize stms
        isVariable Ass _ _ = 1
        isVariable _   _ _ = 0

isOnStack :: AnnotatedStmt -> Int
isOnStack TYP.Cond     exp stm = expOnStack exp + isOnStack stm
isOnStack TYP.CondElse exp stm = expOnStack exp + isOnStack stm
isOnStack TYP.While    exp stm = expOnStack exp + isOnStack stm
isOnStack TYP.SExp     exp     = expOnStack exp
isOnStack _                = 0

expOnStack :: AnnotatedExp -> Int
expOnStack TYP.EVar      _ _ = 1
expOnStack TYP.ELitInt   _ _ = 1
expOnStack TYP.ELitDoub  _ _ = 1
expOnStack TYP.ELitTrue  _ _ = 1
expOnStack TYP.ELitFalse _ _ = 1
expOnStack _         _ _ = 0