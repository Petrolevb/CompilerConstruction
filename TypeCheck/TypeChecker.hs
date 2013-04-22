module TypeChecker where 

import LexJavalette
import ParJavalette
import AbsJavalette

import ErrM
import Control.Monad.State

import AnotatedAbs

import Context
import BuildEnv

type ErrTypeCheck a = StateT Env Err a


typecheck :: Program -> Err AnotatedProgram
typecheck program = evalStateT (anotateCheckProg program) (addIOFun emptyEnv)

createEnv :: Env -> [TopDef] -> Env
createEnv env [t] = extendFun env t
createEnv env (t:ts) = extendFun (createEnv env ts) t

anotateCheckProg :: Program -> ErrTypeCheck AnotatedProgram
anotateCheckProg (Program topdefs) = do
    env <- get
    put $ createEnv env topdefs
    checkMain
    newTopDefs <- sequence $ map typedefs topdefs
    return $ AnotatedProgram  newTopDefs



typedefs :: TopDef -> ErrTypeCheck AnotatedTopDef
typedefs td = do
    env <- get
    put $ updateSignature env ((\(s, _) -> s) (funToSign td))
    checkTopDef td

checkTopDef :: TopDef -> ErrTypeCheck AnotatedTopDef
checkTopDef (AbsJavalette.FnDef t i args block) = do
    anoBlock <- checkBlock block
    return (AnotatedAbs.FnDef t i args anoBlock)


checkBlock :: Block -> ErrTypeCheck AnotatedBlock
checkBlock (Block stmts) = do
    env <- get
    put $ newBlock env
    anoStmts <- checkStmts stmts
    toUpdate <- get
    put $ removeBlock toUpdate
    return (AnotatedBlock anoStmts)


checkStmts :: [Stmt] -> ErrTypeCheck [AnotatedStmt]
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

checkStmt :: Stmt -> ErrTypeCheck AnotatedStmt
checkStmt  AbsJavalette.Empty                      = return AnotatedAbs.Empty
checkStmt (AbsJavalette.VRet)                      = return AnotatedAbs.VRet
checkStmt (AbsJavalette.BStmt block)               = do
    anoBlock <- checkBlock block
    return (AnotatedAbs.BStmt anoBlock)

checkStmt (AbsJavalette.Decl typeDecl items)       = do
    sequence $ map (checkItem typeDecl) items -- update env
    return (AnotatedAbs.Decl typeDecl items)

checkStmt (AbsJavalette.Ass ident expr)            = do
    typeExpr <- infer expr
    anoExp <- checkExp expr
    env <- get
    case lookupVar ident env of
        Bad s1 -> case lookupInFun ident env of
            Bad s2 -> fail (s1 ++ " " ++ s2)
            Ok   t -> if t == typeExpr 
                        then return (AnotatedAbs.Ass ident anoExp) 
                        else fail ("Type Error: " ++ show ident ++ "=" ++ show expr)
        Ok   t -> if t == typeExpr 
                    then return (AnotatedAbs.Ass ident anoExp) 
                    else fail ("Type Error: " ++ show ident ++ "=" ++ show expr)

checkStmt (AbsJavalette.Incr ident)                = do
    env <- get
    case lookupVar ident env of
        Bad s1 -> case lookupInFun ident env of
            Bad s2 -> fail (s1 ++ " " ++ s2)
            Ok   t -> if t == Int 
                        then return (AnotatedAbs.Incr ident) 
                        else fail ("Type Error: " ++ show ident ++ "++")
        Ok   t -> if t == Int 
                    then return (AnotatedAbs.Incr ident) 
                    else fail ("Type Error: " ++ show ident ++ "++")
checkStmt (AbsJavalette.Decr ident)                = do
    env <- get
    case lookupVar ident env of
        Bad s1 -> case lookupInFun ident env of
            Bad s2 -> fail (s1 ++ " " ++ s2)
            Ok   t -> if t == Int 
                        then return (AnotatedAbs.Decr ident) 
                        else fail ("Type Error: " ++ show ident ++ "++")
        Ok   t -> if t == Int 
                    then return (AnotatedAbs.Decr ident) 
                    else fail ("Type Error: " ++ show ident ++ "++")

checkStmt (AbsJavalette.Ret expr)                  = do
    anoExpr <- checkExp expr
    return (AnotatedAbs.Ret anoExpr)
checkStmt (AbsJavalette.Cond expr stmt)            = do
    anoExpr <- checkExp expr
    anoStmt <- checkStmt stmt
    return (AnotatedAbs.Cond anoExpr anoStmt)
checkStmt (AbsJavalette.CondElse expr stmt1 stmt2) = do
    anoExpr <- checkExp expr
    s1 <- checkStmt stmt1
    s2 <- checkStmt stmt2
    return (AnotatedAbs.CondElse anoExpr s1 s2)
checkStmt (AbsJavalette.While expr stmt)           = do
    anoExpr <- checkExp expr
    anoStmt <- checkStmt stmt
    return (AnotatedAbs.While anoExpr anoStmt)

checkExp :: Expr -> ErrTypeCheck AnotatedExp
checkExp = undefined

infer :: Expr -> ErrTypeCheck Type
infer = undefined
