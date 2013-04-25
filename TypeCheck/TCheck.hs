module TCheck where

import ErrM
import Control.Monad.State

import AbsJavalette
import AnnotatedAbs

import Context
import BuildEnv

typeResult :: Bool -> ErrTypeCheck ()
typeResult False = fail "Error on type checking"
typeResult True  = return ()

-- Check type of exp
checkExp :: Expr -> Type -> ErrTypeCheck AnnotatedExp
checkExp ELitTrue      Bool = return (ELitTrue, Bool) 
checkExp ELitFalse     Bool = return (ELitFalse, Bool)
checkExp (ELitInt  i ) Int  = return (ELitInt i, Int)
checkExp (ELitDoub d ) Doub = return (ELitDoub d, Doub)
checkExp (EVar      id) t    = do
         tId <- infer (EVar id)
         typeResult (tId == t)
         return (EVar id, t)
checkExp (EApp id exp) t    = do
         env <- get
         case lookupFun id env of
              Bad s -> do typeResult False
                          return (ELitFalse, Void)
              Ok (tysids,typeFun) ->
                 do typeResult (t == typeFun)
                    -- Same number of argument as requested
                    typeResult (length exp == length tysids)
                    checkAllArgs tysids exp
                    return (EApp id exp, t)
                    where
                        checkAllArgs [] [] = return (ELitTrue, Void)
                        checkAllArgs [(ty, _)] [exp] = 
                                     checkExp exp ty
                        checkAllArgs ((ty,_):tysids) (exp:exps) = do
                                     checkExp exp ty
                                     checkAllArgs tysids exps
checkExp (EString s)   t      = undefined -- return (EString s, String)
checkExp (Neg e)       t      = do
         te <- infer e
         typeResult (te == t)
         return (Neg e, t)
checkExp (Not e) Bool         = do
         te <- infer e
         typeResult (te == Bool)
         return (Not e, Bool)
checkExp (EMul e1 op e2) t    = 
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
         te1 <- checkList e1 e2 [Bool]
         typeResult (te1 == Bool)
         return (ERel e1 op e2, Bool)
checkExp (EAnd e1 e2) Bool    = do
         checkBool e1 e2
         return (EAnd e1 e2, Bool)
checkExp (EOr e1 e2) Bool     = do
         checkBool e1 e2
         return (EOr e1 e2, Bool)
checkExp _ _                  = do typeResult False
                                   return (ELitFalse, Void)

checkList :: Expr -> Expr -> [Type] -> ErrTypeCheck Type
checkList e1 e2 ts = do
        te1 <- infer e1
        checkExp e2 te1
        typeResult (te1 `elem` ts)
        return te1

checkBool :: Expr -> Expr -> ErrTypeCheck Type
checkBool e1 e2 = do
          checkExp e1 Bool
          checkExp e2 Bool
          return Bool

-- Infer type of exp
infer :: Expr -> ErrTypeCheck Type
infer (ELitTrue)     = return Bool
infer (ELitFalse)    = return Bool
infer (ELitInt i)    = return Int
infer (ELitDoub d)   = return Doub
infer (EVar id)      = do
      env <- get
      case lookupVar id env of
           Bad _ -> case lookupInFun id env of
                         Bad _   -> return Void
                         Ok  tId -> return tId
           Ok tId  -> return tId
infer (EApp id exos) = do
      env <- get
      case lookupFun id env of
           Bad _ -> return Void
           Ok (_, typeFun) -> return typeFun
infer (EString s) = undefined -- return (EString s, String)
infer (Neg e)     = do
      te <- infer e
      typeResult (te `elem` [Int, Doub])
      return te
infer (Not e)     = do
      te <- infer e
      typeResult (te == Bool)
      return Bool
infer (EMul e1 op e2) = 
      case op of
           Mod -> do checkList e1 e2 [Int]
                     return Int
           _   -> checkList e1 e2 [Int, Doub]
infer (EAdd e1 op e2) = checkList e1 e2 [Int, Doub]
infer (ERel e1 op e2) = do
      checkList e1 e2 [Bool]
      return Bool
infer (EAnd e1 e2) = checkBool e1 e2
infer (EOr e1 e2)  = checkBool e1 e2

