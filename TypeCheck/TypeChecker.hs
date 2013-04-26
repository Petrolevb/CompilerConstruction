module TypeChecker where 

import LexJavalette
import ParJavalette
import AbsJavalette

import ErrM
import Control.Monad.State

import AnnotatedAbs

import Context
import BuildEnv


typecheck :: Program -> Err AnnotatedProgram
typecheck program = evalStateT (annotateCheckProg program) (addIOFun emptyEnv)

createEnv :: Env -> [TopDef] -> Env
createEnv env [t] = extendFun env t
createEnv env (t:ts) = extendFun (createEnv env ts) t

annotateCheckProg :: Program -> ErrTypeCheck AnnotatedProgram
annotateCheckProg (Program topdefs) = do
    env <- get
    put $ createEnv env topdefs
    checkMain
    newTopDefs <- mapM typedefs topdefs
    return $ AnnotatedProgram  newTopDefs



typedefs :: TopDef -> ErrTypeCheck AnnotatedTopDef
typedefs td = do
    env <- get
    put $ updateSignature env (fst (funToSign td))
    checkTopDef td

checkTopDef :: TopDef -> ErrTypeCheck AnnotatedTopDef
checkTopDef (AbsJavalette.FnDef t i args block) = do
    annoBlock <- checkBlock block
    return (AnnotatedAbs.FnDef t i args annoBlock)


checkBlock :: Block -> ErrTypeCheck AnnotatedBlock
checkBlock (Block stmts) = do
    env <- get
    put $ newBlock env
    annoStmts <- checkStmts stmts
    toUpdate <- get
    put $ removeBlock toUpdate
    return (AnnotatedBlock annoStmts)


checkStmts :: [Stmt] -> ErrTypeCheck [AnnotatedStmt]
checkStmts = mapM checkStmt 
-- checkStmt is the last function of this file

-- Check if the function main is part of the environment
checkMain :: ErrTypeCheck ()
checkMain = do
    env <- get
    check "main" env

check :: String -> Env -> ErrTypeCheck ()
check fun env = 
    case lookupFun (Ident fun) env of
        Bad _ -> fail "TYPE CHECKER ERROR : no main"
        Ok _ -> return ()


checkItem :: Type -> Item -> ErrTypeCheck ()
checkItem typeItem (NoInit ident)   = do
    env <- get
    case extendVar env ident typeItem of
        Ok newEnv -> do 
            put newEnv 
            return ()
        Bad     s -> fail s
checkItem typeItem (Init ident exp) = do
    typeExp <- infer exp 
    if typeExp == typeItem 
        then checkItem typeItem (NoInit ident) -- same case
        else fail ("Type Error: " ++ show ident ++ "=" ++ show exp)

checkStmt :: Stmt -> ErrTypeCheck AnnotatedStmt
checkStmt  AbsJavalette.Empty                      = return AnnotatedAbs.Empty
checkStmt (AbsJavalette.VRet)                      = return AnnotatedAbs.VRet
checkStmt (AbsJavalette.BStmt block)               = do
    annoBlock <- checkBlock block
    return (AnnotatedAbs.BStmt annoBlock)

checkStmt (AbsJavalette.Decl typeDecl items)       = do
    mapM_ (checkItem typeDecl) items -- update env
    return (AnnotatedAbs.Decl typeDecl items)

checkStmt (AbsJavalette.Ass ident expr)            = do
    typeExpr <- infer expr
    annoExp <- checkExp expr typeExpr
    env <- get
    case lookupVar ident env of
        Bad s1 -> case lookupInFun ident env of
            Bad s2 -> fail (s1 ++ " " ++ s2)
            Ok   t -> if t == typeExpr 
                        then return (AnnotatedAbs.Ass ident annoExp) 
                        else fail ("Type Error: " ++ show ident ++ "=" ++ show expr)
        Ok   t -> if t == typeExpr 
                    then return (AnnotatedAbs.Ass ident annoExp) 
                    else fail ("Type Error: " ++ show ident ++ "=" ++ show expr)

checkStmt (AbsJavalette.Incr ident)                = do
    env <- get
    case lookupVar ident env of
        Bad s1 -> case lookupInFun ident env of
            Bad s2 -> fail (s1 ++ " " ++ s2)
            Ok   t -> if t == Int 
                        then return (AnnotatedAbs.Incr ident) 
                        else fail ("Type Error: " ++ show ident ++ "++")
        Ok   t -> if t == Int 
                    then return (AnnotatedAbs.Incr ident) 
                    else fail ("Type Error: " ++ show ident ++ "++")
checkStmt (AbsJavalette.Decr ident)                = do
    env <- get
    case lookupVar ident env of
        Bad s1 -> case lookupInFun ident env of
            Bad s2 -> fail (s1 ++ " " ++ s2)
            Ok   t -> if t == Int 
                        then return (AnnotatedAbs.Decr ident) 
                        else fail ("Type Error: " ++ show ident ++ "++")
        Ok   t -> if t == Int 
                    then return (AnnotatedAbs.Decr ident) 
                    else fail ("Type Error: " ++ show ident ++ "++")

checkStmt (AbsJavalette.Ret expr)                  = do
    t <- infer expr
    annoExpr <- checkExp expr t
    return (AnnotatedAbs.Ret annoExpr)
checkStmt (AbsJavalette.Cond expr stmt)            = do
    t <- infer expr
    annoExpr <- checkExp expr t
    annoStmt <- checkStmt stmt
    return (AnnotatedAbs.Cond annoExpr annoStmt)
checkStmt (AbsJavalette.CondElse expr stmt1 stmt2) = do
    t <- infer expr
    annoExpr <- checkExp expr t
    s1 <- checkStmt stmt1
    s2 <- checkStmt stmt2
    return (AnnotatedAbs.CondElse annoExpr s1 s2)
checkStmt (AbsJavalette.While expr stmt)           = do
    t <- infer expr
    annoExpr <- checkExp expr t
    annoStmt <- checkStmt stmt
    return (AnnotatedAbs.While annoExpr annoStmt)
checkStmt (AbsJavalette.SExp expr) = do
    t <- infer expr
    annoExpr <- checkExp expr t
    return (AnnotatedAbs.SExp annoExpr)

typeResult :: Bool -> String -> ErrTypeCheck ()
typeResult False s = fail $ "Type Error : " ++ s
typeResult True  _ = return ()

-- Check type of exp
checkExp :: Expr -> Type -> ErrTypeCheck AnnotatedExp
checkExp ELitTrue      Bool = return (ELitTrue, Bool) 
checkExp ELitFalse     Bool = return (ELitFalse, Bool)
checkExp (ELitInt  i ) Int  = return (ELitInt i, Int)
checkExp (ELitDoub d ) Doub = return (ELitDoub d, Doub)
checkExp (EVar      id) t    = do
         tId <- infer (EVar id)
         typeResult (tId == t) "check exp Evar"
         return (EVar id, t)
checkExp (EApp id exp) t    = do
         env <- get
         case lookupFun id env of
              Bad s -> do
                    typeResult False ("checkexp EApp : lookupFun fail " ++ s)
                    return (ELitFalse, Void)
              Ok (tysids,typeFun) ->
                 do typeResult (t == typeFun) "checkExp EApp"
                    -- Same number of argument as requested
                    typeResult (length exp == length tysids) "checkExp EApp : length exp /= length args"
                    checkAllArgs tysids exp
                    return (EApp id exp, t)
                    where
                        checkAllArgs [] [] = return (ELitTrue, Void)
                        checkAllArgs [(ty, _)] [exp] = 
                                     checkExp exp ty
                        checkAllArgs ((ty,_):tysids) (exp:exps) = do
                                     checkExp exp ty
                                     checkAllArgs tysids exps
checkExp (EString s)   t      = return (EString s, Str) 
checkExp (Neg e)       t      = do
         te <- infer e
         typeResult (te == t) "checkExp Neg"
         return (Neg e, t)
checkExp (Not e) Bool         = do
         te <- infer e
         typeResult (te == Bool) "checkExp Not"
         return (Not e, Bool)
checkExp (EMul e1 op e2) t    = 
         case op of
              Mod -> do te1 <- checkList e1 e2 [Int]
                        typeResult (te1 == t) "checkExp EMul"
                        return (EMul e1 op e2, t)
              _   -> do te1 <- checkList e1 e2 [Int, Doub]
                        typeResult (te1 == t) "checkExp Emul"
                        return (EMul e1 op e2, t)
checkExp (EAdd e1 op e2) t    = do
         te1 <- checkList e1 e2 [Int, Doub]
         typeResult(te1 == t) "checkExp EAdd"
         return (EAdd e1 op e2, t)
checkExp (ERel e1 op e2) Bool = do
         te1 <- checkList e1 e2 [Int, Doub, Bool]
         return (ERel e1 op e2, Bool)
checkExp (EAnd e1 e2) Bool    = do
         checkBool e1 e2
         return (EAnd e1 e2, Bool)
checkExp (EOr e1 e2) Bool     = do
         checkBool e1 e2
         return (EOr e1 e2, Bool)
checkExp _ _                  = do typeResult False  "checkExp inconnu"
                                   return (ELitFalse, Void)

checkList :: Expr -> Expr -> [Type] -> ErrTypeCheck Type
checkList e1 e2 ts = do
        te1 <- infer e1
        checkExp e2 te1
        typeResult (te1 `elem` ts) ("check List " ++ show te1 ++ " not in " ++ show ts)
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
infer (EString s) = return Str
infer (Neg e)     = do
      te <- infer e
      typeResult (te `elem` [Int, Doub]) "infer Neg"
      return te
infer (Not e)     = do
      te <- infer e
      typeResult (te == Bool) "infer not"
      return Bool
infer (EMul e1 op e2) = 
      case op of
           Mod -> do checkList e1 e2 [Int]
                     return Int
           _   -> checkList e1 e2 [Int, Doub]
infer (EAdd e1 op e2) = checkList e1 e2 [Int, Doub]
infer (ERel e1 op e2) = do
      checkList e1 e2 [Int, Doub, Bool]
      return Bool
infer (EAnd e1 e2) = checkBool e1 e2
infer (EOr e1 e2)  = checkBool e1 e2

