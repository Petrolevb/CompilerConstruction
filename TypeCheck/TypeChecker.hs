module TypeChecker where 

import LexJavalette
import ParJavalette
import AbsJavalette

import ErrM
import Control.Monad.State

import AnnotatedAbs
import TCheck

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
    newTopDefs <- sequence $ map typedefs topdefs
    return $ AnnotatedProgram  newTopDefs



typedefs :: TopDef -> ErrTypeCheck AnnotatedTopDef
typedefs td = do
    env <- get
    put $ updateSignature env ((\(s, _) -> s) (funToSign td))
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
checkStmts stmts = sequence $ map checkStmt stmts
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
    sequence $ map (checkItem typeDecl) items -- update env
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
