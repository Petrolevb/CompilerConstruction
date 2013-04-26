-- ldc    -> +1
-- load  -> +1
-- store -> +1

getLocalaStackSize :: AnnotatedBlock -> (Int, Int)
getLocalaStackSize [(stm, t):stms] = ((isVariable stm)+ fst(getLocalaStackSize stms),
                                      (isOnStack stm) + snd(getLocalaStackSize stms))
getLocalaStackSize [] = (0, 0)
  where isVariable s = undefined