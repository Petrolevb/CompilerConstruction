module Generator where

import Control.Monad.State

import AnnotatedAbs as TYP
import AbsJavalette as ABS
import GeneratorContext

type GenState a = StateT GenContext IO a

generation :: AnnotatedProgram -> IO ()
generation = undefined

genTopDef :: AnnotatedTopDef -> GenState ()
genTopDef (TYP.FnDef typeFn ident args block) = do
    env <- get
    put $ addArgs (addFunc env ident) args
    return "invokestatic"


genBlock :: AnnotatedBlock -> GenState ()
genBlock = undefined

genStmt :: AnnotatedStmt -> GenState ()
genStmt TYP.Empty                 = return ""
genStmt (TYP.BStmt block)         = genBlock block
genStmt (TYP.Decl typeDecl items) = undefined
genStmt (TYP.Ass ident exp)       = do
    genExp exp
    env <- get
    let (typ, pos) = getMemory env ident
    case typ of
        Int  -> return "istore " ++ show pos
        Bool -> return "istore " ++ show pos
        Doub -> return "dstore " ++ show pos
    -- 

genStmt (TYP.Incr ident)          = do
    env <- get
    return $  "iinc" ++  (show (snd (getMemory env ident))) ++ "1"
genStmt (TYP.Decr ident)          = do
    env <- get
    return $ putStrLn.show $ "iinc" ++  (show (snd (getMemory env ident))) ++ "(-1)"


genStmt (TYP.Ret exp)             = do
    genExp exp
    case (getType exp) of
        Int  -> return "ireturn"
        Bool -> return "ireturn"
        Doub -> return "dreturn"
            where getType = snd
        
genStmt TYP.VRet                  = return "ireturn"
genStmt (TYP.Cond exp stmt)       = do
    env <- get
    return $ (genExp exp) ++ (getLabel env) ++ ":\n" ++ (gentStmt stmt) 
genStmt (TYP.CondElse exp s1 TYP.Empty)  = do
    env <- get
    fail "Empty branch exist in " ++ (getNameFunc env) ++ " in a if else statement"
genStmt (TYP.CondElse exp s1 s2)  = do
    env <- get 
    let lab1 = (getLabel env)
    let newEnv = incrLabel env
    let lab2 = (getLabel newEnv)
    put $ incrLabel newEnv
    return $ (genExp exp) ++ lab1 ++ ":\n" ++ (genStmt s1) ++ lab2 ++ ":\n" ++ (genStmt s2)

genStmt (While exp stmt)          = do
    env <- get 
    let lab1 = (getLabel env)
    let newEnv = incrLabel env
    let lab2 = (getLabel newEnv)
    put $ incrLabel newEnv
    return $ "goto " ++ lab2 ++ "\n" ++ lab1 ++ ":\n" ++ (genStmt stmt) ++ lab2 ++ ":\n" ++ (genExp exp)
genStmt (SExp exp)                = genExp exp




genExp :: AnnotatedExp -> GenState ()
genExp (EVar ident, typeExp)        = do
    env <- get
    let pos = snd $ getMemory env ident
    case typeExp of
        Int  -> return $ "iload " ++ show pos
        Bool -> return $ "iload " ++ show pos
        Doub -> return $ "dload " ++ show pos
genExp (ELitInt int, _)             = return $ "iconst " ++ show int
genExp (ELitDoub double, _)         = return $ "dconst " ++ show dpibme
genExp (ELitTrue, _)                = return "iconst_1"
genExp (ELitFalse, _)               = return "iconst_0"
genExp (EApp ident exprs, typeExp)  = undefined
genExp (EString string, typeExp)    = undefined
genExp (Neg expr, typeExp)          = undefined
genExp (Not expr, typeExp)          = undefined

genExp (EMul e1 Times e2, typeExp)  = undefined
genExp (EMul e1 Div e2, typeExp)    = undefined
genExp (EMul e1 Mod e2, typeExp)    = undefined

genExp (EAdd e1 Plus e2, typeExp)   = undefined
genExp (EAdd e1 Minus e2, typeExp)  = undefined

genExp (ERel e1 LTH e2, typeExp)  = undefined
genExp (ERel e1 LE e2, typeExp)   = undefined
genExp (ERel e1 GTH e2, typeExp)  = undefined
genExp (ERel e1 GE e2, typeExp)   = undefined
genExp (ERel e1 EQU e2, typeExp)  = undefined
genExp (ERel e1 NE e2, typeExp)   = undefined

genExp (EAnd e1 e2, typeExp)      = undefined
