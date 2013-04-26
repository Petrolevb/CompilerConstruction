module Generator where

import Control.Monad.State

import AnnotatedAbs as TYP
import AbsJavalette as ABS
import GeneratorContext

type GenState a = StateT GenContext IO a 

returnCode :: String -> GenState ()
returnCode = liftIO.(appendFile "a.out")

getType :: AnnotatedExp -> Type
getType = snd

getLetterFromType :: Type -> Char
getLetterFromType t = case t of
    Int -> 'I'
    Doub -> 'D'
    Void -> 'V'
    Bool -> 'I'

getLettersArgs :: [Arg] -> String
getLettersArgs  = map (\(Arg typeA _) -> getLetterFromType typeA)

generation :: AnnotatedProgram -> IO ()
generation (AnnotatedProgram topdefs) = evalStateT (genProg topdefs) newContext

genProg :: [AnnotatedTopDef] -> GenState ()
genProg topdefs = mapM_  genTopDef topdefs

genTopDef :: AnnotatedTopDef -> GenState ()
genTopDef (TYP.FnDef typeFn ident args block) = do
    env <- get
    put $ addArgs (addFunc env ident) args
    env <- get
    let (local, stack) = getLocalaStackSize block
    returnCode $ ".method public static " ++ (getNameFunc env) 
    returnCode $ "(" ++ (getLettersArgs args) ++ ")" ++ (getLetterFromType typeFn) : "\n" 
    returnCode $ ".limit locals " ++ (show local) ++ "\n"
    returnCode $ ".limit stack " ++ (show stack) ++ "\nentry:\n" 
    genBlock block
    returnCode $ ".end method\n\n"

genBlock :: AnnotatedBlock -> GenState ()
genBlock (AnnotatedBlock stmts) = mapM_ genStmt stmts

genStmt :: AnnotatedStmt -> GenState ()
genStmt TYP.Empty                 = returnCode ""
genStmt (TYP.BStmt block)         = genBlock block
genStmt (TYP.Decl typeDecl items) = genDecl typeDecl items

genStmt (TYP.Ass ident exp)       = do
    genExp exp
    env <- get
    let (typ, pos) = getMemory env ident
    case typ of
        Int  -> returnCode $ "istore " ++ show pos ++ "\n"
        Bool -> returnCode $ "istore " ++ show pos ++ "\n"
        Doub -> returnCode $ "dstore " ++ show pos ++ "\n"
    -- 

genStmt (TYP.Incr ident)          = do
    env <- get
    returnCode $  "iinc" ++  (show (snd (getMemory env ident))) ++ "1\n"
genStmt (TYP.Decr ident)          = do
    env <- get
    returnCode $ "iinc" ++  (show (snd (getMemory env ident))) ++ "(-1)\n"


genStmt (TYP.Ret exp)             = do
    genExp exp
    case (getType exp) of
        Int  -> returnCode "ireturn\n"
        Bool -> returnCode "ireturn\n"
        Doub -> returnCode "dreturn\n"
        
genStmt TYP.VRet                  = returnCode "ireturn\n"
genStmt (TYP.Cond (ELitTrue, _) stmt)  = genStmt stmt
genStmt (TYP.Cond (ELitFalse, _) stmt) = returnCode ""
genStmt (TYP.Cond exp stmt)       = do
    env <- get
    genExp exp
    let lab1 = getLabel env
    put $ incrLabel env
    returnCode $ lab1 ++ ":\n" 
    genStmt stmt
genStmt (TYP.CondElse exp s1 TYP.Empty)  = do
    env <- get
    fail $ "Empty branch exist in " ++ (getNameFunc env) ++ " in a if else statement"
genStmt (TYP.CondElse (ELitTrue, _) s _)  = genStmt s
genStmt (TYP.CondElse (ELitFalse, _) _ s) = genStmt s
genStmt (TYP.CondElse exp s1 s2)  = do
    env <- get 
    let lab1 = (getLabel env)
    let newEnv = incrLabel env
    let lab2 = (getLabel newEnv)
    put $ incrLabel newEnv
    genExp exp
    returnCode $ " " ++ lab1 ++ "\n" -- if true jump lab1, else continue
    genStmt s2
    returnCode $ "goto " ++ lab2 ++ "\n"
    returnCode $ lab1 ++ ":\n" 
    genStmt s1
    returnCode $ lab2 ++ ":\n" 

genStmt (TYP.While exp stmt)          = do
    env <- get 
    let lab1 = (getLabel env)
    let newEnv = incrLabel env
    let lab2 = (getLabel newEnv)
    put $ incrLabel newEnv
    returnCode $ "goto " ++ lab2 ++ "\n" ++ lab1 ++ ":\n" 
    genStmt stmt
    returnCode $ lab2 ++ ":\n" 
    genExp exp
    returnCode $ " " ++ lab1 ++"\n" -- finish on an if (theorically)
genStmt (TYP.SExp exp)                = genExp exp




genExp :: AnnotatedExp -> GenState ()
genExp (EVar ident, typeExp)        = do
    env <- get
    let pos = snd $ getMemory env ident
    case typeExp of
        Int  -> returnCode $ "iload " ++ show pos ++ "\n"
        Bool -> returnCode $ "iload " ++ show pos ++ "\n"
        Doub -> returnCode $ "dload " ++ show pos ++ "\n"
genExp (ELitInt int, _)             = returnCode $ "ldc " ++ show int ++ "\n"
genExp (ELitDoub double, _)         = returnCode $ "ldc2_w " ++ show double ++ "\n"
genExp (ELitTrue, _)                = returnCode $ "iconst_1" ++ "\n"
genExp (ELitFalse, _)               = returnCode $ "iconst_0" ++" \n"
genExp (EApp ident exprs, typeExp)  = returnCode "EApp\n"
genExp (EString string, typeExp)    = returnCode "EString\n"
genExp (Neg expr, typeExp)          = do
    genExp (expr, typeExp)
    returnCode "ineg\n"
genExp (Not expr, typeExp)          = returnCode "Not\n"

genExp (EMul e1 Times e2, typeExp)  = do
    genExp (e1, typeExp)
    genExp (e2, typeExp)
    case typeExp of
        Int  -> returnCode "imul"
        Doub -> returnCode "dmul"
genExp (EMul e1 Div e2, typeExp)    = do
    genExp (e1, typeExp)
    genExp (e2, typeExp)
    case typeExp of
        Int  -> returnCode "idiv"
        Doub -> returnCode "ddiv"
genExp (EMul e1 Mod e2, typeExp)    = do
    genExp (e1, typeExp)
    genExp (e2, typeExp)
    returnCode "irem" 
genExp (EAdd e1 Plus e2, typeExp)   = do
    genExp (e1, typeExp)
    genExp (e2, typeExp)
    case typeExp of
        Int  -> returnCode "iadd\n"
        Doub -> returnCode "dadd\n"
genExp (EAdd e1 Minus e2, typeExp)  = do
    genExp (e1, typeExp)
    genExp (e2, typeExp)
    case typeExp of
        Int  -> returnCode "isub\n"
        Doub -> returnCode "dsub\n"

genExp (ERel e1 LTH e2, typeExp)  = case typeExp of
    Int  -> genConditionInt e1 e2 "if_icmplt"
    Doub -> genConditionDouble e1 e2 "dcmpl" "iflt"
genExp (ERel e1 LE e2, typeExp)   = case typeExp of
    Int  -> genConditionInt e1 e2 "if_icmple"
    Doub -> genConditionDouble e1 e2 "dcmpl" "ifle"
genExp (ERel e1 GTH e2, typeExp)  = case typeExp of
    Int  -> genConditionInt e1 e2 "if_icmpgt"
    Doub -> genConditionDouble e1 e2 "dcmpg" "ifgt"
genExp (ERel e1 GE e2, typeExp)   = case typeExp of
    Int  -> genConditionInt e1 e2 "if_icmpge"
    Doub -> genConditionDouble e1 e2 "dcmpg" "ifge"
genExp (ERel e1 EQU e2, typeExp)  = case typeExp of
    Int  -> genConditionInt e1 e2 "if_icmpeq"
    Doub -> genConditionDouble e1 e2 "dcmpl" "ifeq"
genExp (ERel e1 NE e2, typeExp)   = case typeExp of
    Int  -> genConditionInt e1 e2 "if_icmpne"
    Doub -> genConditionDouble e1 e2 "dcmpl" "ifne"

genExp (EAnd e1 e2, typeExp)      = returnCode "EAnd\n"
genExp (EOr e1 e2, typeExp)       = returnCode "EOr\n"

genConditionDouble :: Expr -> Expr -> String -> String -> GenState ()
genConditionDouble e1 e2 s1 s2= do
    genExp (e1, Doub)
    genExp (e2, Doub)
    returnCode $ s1 ++ "\n" ++ s2


genConditionInt :: Expr -> Expr -> String -> GenState ()
genConditionInt e1 e2 s = do
    genExp (e1, Int)
    genExp (e2, Int)
    returnCode s


genDecl :: Type -> [Item] -> GenState ()
genDecl t is = mapM_  (genItem t) is
    where 
        genItem t (NoInit ident)    = do
            env <- get
            put $ addVar env (t, ident)
        genItem t (Init ident exp)  = do
            genExp (exp, t)
            env <- get
            put $ addVar env (t, ident)
            env <- get
            let (_, pos) = getMemory env ident
            case t of 
                Int  -> returnCode $ "istore " ++ show pos ++ "\n"
                Bool -> returnCode $ "istore " ++ show pos ++ "\n" 
                Doub -> returnCode $ "dstore " ++ show pos ++ "\n"
