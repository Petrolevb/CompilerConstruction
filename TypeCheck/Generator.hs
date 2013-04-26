module Generator where

import Control.Monad.State

import AnnotatedAbs as TYP
import AbsJavalette as ABS

import GeneratorContext
import Size

type GenState a = StateT GenContext IO a 

returnCode :: String -> GenState ()
returnCode = liftIO.(appendFile "a.out")


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
genStmt (TYP.Cond (TYP.ELitTrue _) stmt)  = genStmt stmt
genStmt (TYP.Cond (TYP.ELitFalse _) stmt) = returnCode ""
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
genStmt (TYP.CondElse (TYP.ELitTrue _) s _)  = genStmt s
genStmt (TYP.CondElse (TYP.ELitFalse _) _ s) = genStmt s
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
genExp (TYP.EVar ident typeExp)        = do
    env <- get
    let pos = snd $ getMemory env ident
    case typeExp of
        Doub -> returnCode $ "dload " ++ show pos ++ "\n"
        _    -> returnCode $ "iload " ++ show pos ++ "\n"
genExp (TYP.ELitInt int _)             = returnCode $ "ldc " ++ show int ++ "\n"
genExp (TYP.ELitDoub double _)         = returnCode $ "ldc2_w " ++ show double ++ "\n"
genExp (TYP.ELitTrue _)                = returnCode $ "iconst_1" ++ "\n"
genExp (TYP.ELitFalse _)               = returnCode $ "iconst_0" ++" \n"
genExp (TYP.EApp ident exprs typeExp)  = returnCode "EApp\n"
genExp (TYP.EString string typeExp)    = returnCode "EString\n"
genExp (TYP.Neg expr typeExp)          = do
    genExp expr
    returnCode "ineg\n"
genExp (TYP.Not expr typeExp)          = returnCode "Not\n"
genExp (TYP.EMul e1 Times e2 typeExp)  = do
    genExp e1
    genExp e2 
    case typeExp of
        Int  -> returnCode "imul"
        Doub -> returnCode "dmul"
genExp (TYP.EMul e1 Div e2 typeExp)    = do
    genExp e1 
    genExp e2
    case typeExp of
        Int  -> returnCode "idiv"
        Doub -> returnCode "ddiv"
genExp (TYP.EMul e1 Mod e2 typeExp)    = do
    genExp e1
    genExp e2
    returnCode "irem" 
genExp (TYP.EAdd e1 Plus e2 typeExp)   = do
    genExp e1
    genExp e2
    case typeExp of
        Int  -> returnCode "iadd\n"
        Doub -> returnCode "dadd\n"
genExp (TYP.EAdd e1 Minus e2 typeExp)  = do
    genExp e1
    genExp e2
    case typeExp of
        Int  -> returnCode "isub\n"
        Doub -> returnCode "dsub\n"

genExp (TYP.ERel e1 LTH e2 t)  = do
    typeExp <- getType e1
    case typeExp of
        Int  -> genConditionInt e1 e2 "if_icmplt"
        Doub -> genConditionDouble e1 e2 "dcmpl" "iflt"
genExp (TYP.ERel e1 LE e2 t)   = do 
    typeExp <- getType e1
    case typeExp of
        Int  -> genConditionInt e1 e2 "if_icmple"
        Doub -> genConditionDouble e1 e2 "dcmpl" "ifle"
genExp (TYP.ERel e1 GTH e2 t)  = do 
    typeExp <- getType e1
    case typeExp of
        Int  -> genConditionInt e1 e2 "if_icmpgt"
        Doub -> genConditionDouble e1 e2 "dcmpg" "ifgt"
genExp (TYP.ERel e1 GE e2 t)   = do 
    typeExp <- getType e1
    case typeExp of
        Int  -> genConditionInt e1 e2 "if_icmpge"
        Doub -> genConditionDouble e1 e2 "dcmpg" "ifge"
genExp (TYP.ERel e1 EQU e2 t)  = do 
    typeExp <- getType e1
    case typeExp of
        Doub -> genConditionDouble e1 e2 "dcmpl" "ifeq"
        _    -> genConditionInt e1 e2 "if_icmpeq"
genExp (TYP.ERel e1 NE e2 t)   = do 
    typeExp <- getType e1
    case typeExp of
        Doub -> genConditionDouble e1 e2 "dcmpl" "ifne"
        _    -> genConditionInt e1 e2 "if_icmpne"


genExp (TYP.EAnd e1 e2 typeExp)      = returnCode "EAnd\n"
genExp (TYP.EOr e1 e2 typeExp)       = returnCode "EOr\n"

genConditionDouble :: AnnotatedExp -> AnnotatedExp -> String -> String -> GenState ()
genConditionDouble e1 e2 s1 s2= do
    genExp e1
    genExp e2
    returnCode $ s1 ++ "\n" ++ s2

genConditionInt :: AnnotatedExp -> AnnotatedExp -> String -> GenState ()
genConditionInt e1 e2 s = do
    genExp e1
    genExp e2
    returnCode s


genDecl :: Type -> [AnnotatedItem] -> GenState ()
genDecl t is = mapM_  (genItem t) is
    where 
        genItem t (TYP.NoInit ident)    = do
            env <- get
            put $ addVar env (t, ident)
        genItem t (TYP.Init ident exp)  = do
            genExp exp
            env <- get
            put $ addVar env (t, ident)
            env <- get
            let (_, pos) = getMemory env ident
            case t of 
                Int  -> returnCode $ "istore " ++ show pos ++ "\n"
                Bool -> returnCode $ "istore " ++ show pos ++ "\n" 
                Doub -> returnCode $ "dstore " ++ show pos ++ "\n"


getType :: AnnotatedExp -> Type
getType (TYP.EVar     _ t) = t
getType (TYP.ELitInt  _ t) = t
getType (TYP.ELitDoub _ t) = t
getType (TYP.ELitTrue t)   = t
getType (TYP.ELitFalse t)  = t
getType (TYP.EApp _ _ t)   = t
getType (TYP.EString _ t)  = t
getType (TYP.Neg _ t)      = t
getType (TYP.Not _ t)      = t
getType (TYP.EMul _ _ _ t) = t
getType (TYP.EAdd _ _ _ t) = t
getType (TYP.ERel _ _ _ t) = t
getType (TYP.EAnd _ _ t)   = t
getType (TYP.EOr _ _ t)    = t
