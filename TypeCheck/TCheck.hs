module TCheck where

import AnnotatedAbs

typeResult :: Bool -> Err ()
typeResult False = Bad "Error on type checking"
typeResult True  = Ok ()

-- Check type of exp
checkExp :: Exp -> Type -> ErrTypeCheck Annotated
checkExp ELitTrue      Bool = return (ELitTrue, Bool) 
checkExp ELitFalse     Bool = return (ElitFalse, Bool)
checkExp (ELitInt  i ) Int  = return (ELitInt i, Int)
checkExp (ELitDoub d ) Doub = return (ELitDoub d, Doub)
checkExp (EId      id) t    = do
         (_, tId) <- infer (EId id)
         typeResult (tId == t)
         return (EId id, t)
checkExp (EApp id exp) t    = do
         env <- get
         (tysids,typeFun) <- lookupFun id env
         -- Function return same type as requested
         typeResult (t == typeFun)
         -- Same number of argument as requested
         typeResult (length exp == length tysids)
         checkAllArgs tysids exp
         return (EApp id exp, t)
             where
                checkAllArgs [] [] = Ok ()
                checkAllArgs [(ty, _)] [exp] = 
                             checkExp exp ty
                checkAllArgs ((ty,_):tysids) (exp:exps) = do
                             checkExp exp ty
                             checkAllArgs tysids exps
checkExp (EString s)   String = return (EString s, String)
checkExp (Neg e)       t      = do
         (_, te) <- infer e
         typeResult (te == t)
         return (Neg e, t)
checkExp (Not e) Bool         = do
         (_, te) <- infer e
         typeResult (te == t)
         return (Not e, t)
checkExp (EMul e1 op e2) t    = do
         case op of
              Mod -> do te1 <- checkList e1 e2 [Int]
                        typeResult (te1 == t)
                        return (EMul e1 op e2, t)
              _   -> do te1 <- checkList e1 e2 [Int, Doub]
                        typeResult (te1 == t)
                        return (EMul e1 op e2, t)
checkExp (EAdd e1 op e2) t    = do
         te1 <- checkList e1 e2 [Int, Doub]
         typeResult(te1 == t)
         return (EAdd e1 op e2, t)
checkExp (ERel e1 op e2) Bool = do
         t <- checkList e1 e2 [Bool]
         typeResult (t == Bool)
         return (ERel e1 op e2, Bool)
checkExp (EAnd e1 e2) Bool    = do
         checkBool e1 e2
         return (EAnd e1 e2, Bool)
checkExp (EOr e1 e2) Bool     = do
         checkBool e1 e2
         return (EOr e1 e2, Bool)
checkExp _ _                  = typeResult False

checkList :: Exp -> Exp -> [Type] -> Err Type
checkList e1 e2 ts = do
        (_, te1) <- infer e1
        checkExp e2 te1
        typeResult (te1 `elem` ts)
        Ok te1

checkBool :: Exp -> Exp -> Err Type
checkBool e1 e2 = do
          checkExp e1 Bool
          checkExp e2 Bool

-- Infer type of exp
infer :: Exp -> ErrTypeCheck Type
infer (ELitTrue)     = return (ELitTrue, Bool)
infer (ELitFalse)    = return (ELitFalse, Bool)
infer (ELitInt i)    = return (ELitInt i, Int)
infer (ELitDoub d)   = return (ELitDoub d, Doub)
infer (EVar id)      = do
      env <- get
      case lookupVar id env of
           Bad _ -> do t <- lookupInFun id env
                       return (Evar id, t)
           Ok t  -> return (EVar id, t)
infer (EApp id exos) = do
      env <- get
      (_, typeFun) <- lookupFun id env
      return (EApp id exos, typeFun)
infer (EString s) = return (EString s, String)
infer (Neg e)     = do
      (_, t) <- infer e
      typeResult (t `elem` [Int, Doub])
      return (Neg e, t)
infer (Not e)     = do
      t <- infer e
      typeResult (t == Bool)
      return (Not e, Bool)
infer (EMul e1 op e2) = do
      case op of
           Mod -> do checkList e1 e2 [Int]
                     return (EMul e1 op e2, Int)
           _   -> do te1 <- checkList e1 e2 [Int, Doub]
                     return (EMul e1 op e2, te1)
infer (EAdd e1 op e2) = do
      te1 <- checkList e1 e2 [Int, Doub]
      return (EAdd e1 op e2, te1)
infer (ERel e1 op e2) = do
      checkList e1 e2 [Bool]
      return (ERel e1 op e2, Bool)
infer (EAnd e1 e2) = do
      checkBool e1 e2
      return (EAnd e1 e2, Bool)
infer (EOr e1 e2)  = do
      checkBool e1 e2
      return (EOr e1 e2)

