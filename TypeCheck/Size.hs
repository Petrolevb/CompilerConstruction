-- ldc    -> +1
-- bipush -> +1
-- const  -> +1
-- load   -> +1
-- store  -> -1

getLocalaStackSize :: AnnotatedBlock -> (Int, Int)
getLocalaStackSize [(stm, t):stms] = ((isVariable stm) + fst(getLocalaStackSize stms),
                                      (isOnStack stm)  + snd(getLocalaStackSize stms))
getLocalaStackSize [] = (0, 0)
  where isVariable Ass _ _ = 1
        isVariable _   _ _ = 0
        isOnStack _ = undefined