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

getLocalaStackSize :: AnnotatedBlock -> (Int, Int)
getLocalaStackSize _ = (0, 0)

generation :: AnnotatedProgram -> IO ()
generation (AnnotatedProgram topdefs) = evalStateT (genProg topdefs) newContext

genProg :: [AnnotatedTopDef] -> GenState ()
genProg topdefs = mapM_  genTopDef topdefs

genTopDef :: AnnotatedTopDef -> GenState ()
genTopDef (TYP.FnDef typeFn ident args block) = do
    env <- get
    put $ addArgs (addFunc env ident) args
    let (local, stack) = getLocalaStackSize block
    returnCode $ ".method public static " ++ (getNameFunc env) 
    returnCode $ "(" ++ (getLettersArgs args) ++ ")" ++ (getLetterFromType typeFn) : "\n" 
    returnCode $ ".limit locals " ++ (show local) ++ "\n"
    returnCode $ ".limit stack " ++ (show stack) ++ "\nentry:\n" 
    genBlock block
    returnCode $ ".end method"

genBlock :: AnnotatedBlock -> GenState ()
genBlock (AnnotatedBlock stmts) = mapM_ genStmt stmts

genStmt :: AnnotatedStmt -> GenState ()
genStmt TYP.Empty                 = returnCode ""
genStmt (TYP.BStmt block)         = genBlock block
genStmt (TYP.Decl typeDecl items) = undefined
genStmt (TYP.Ass ident exp)       = do
    genExp exp
    env <- get
    let (typ, pos) = getMemory env ident
    case typ of
        Int  -> returnCode $ "istore " ++ show pos
        Bool -> returnCode $ "istore " ++ show pos
        Doub -> returnCode $ "dstore " ++ show pos
    -- 

genStmt (TYP.Incr ident)          = do
    env <- get
    returnCode $  "iinc" ++  (show (snd (getMemory env ident))) ++ "1"
genStmt (TYP.Decr ident)          = do
    env <- get
    returnCode $ "iinc" ++  (show (snd (getMemory env ident))) ++ "(-1)"


genStmt (TYP.Ret exp)             = do
    genExp exp
    case (getType exp) of
        Int  -> returnCode "ireturn"
        Bool -> returnCode "ireturn"
        Doub -> returnCode "dreturn"
        
genStmt TYP.VRet                  = returnCode "ireturn"
genStmt (TYP.Cond exp stmt)       = do
    env <- get
    genExp exp
    let lab1 = getLabel env
    returnCode $ lab1 ++ ":\n" 
    genStmt stmt
genStmt (TYP.CondElse exp s1 TYP.Empty)  = do
    env <- get
    fail $ "Empty branch exist in " ++ (getNameFunc env) ++ " in a if else statement"
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
        Int  -> returnCode $ "iload " ++ show pos
        Bool -> returnCode $ "iload " ++ show pos
        Doub -> returnCode $ "dload " ++ show pos
genExp (ELitInt int, _)             = returnCode $ "iconst " ++ show int
genExp (ELitDoub double, _)         = returnCode $ "dconst " ++ show double
genExp (ELitTrue, _)                = returnCode "iconst_1"
genExp (ELitFalse, _)               = returnCode "iconst_0"
genExp (EApp ident exprs, typeExp)  = undefined
genExp (EString string, typeExp)    = undefined
genExp (Neg expr, typeExp)          = undefined
genExp (Not expr, typeExp)          = undefined

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
        Int  -> returnCode "iadd"
        Doub -> returnCode "dadd"
genExp (EAdd e1 Minus e2, typeExp)  = do
    genExp (e1, typeExp)
    genExp (e2, typeExp)
    case typeExp of
        Int  -> returnCode "isub"
        Doub -> returnCode "dsub"

genExp (ERel e1 LTH e2, typeExp)  = case typeExp of
    Int  -> genCondition typeExp e1 e2 "if_icmplt"
    Doub -> undefined
genExp (ERel e1 LE e2, typeExp)   = case typeExp of
    Int  -> genCondition typeExp e1 e2 "if_icmple"
    Doub -> undefined
genExp (ERel e1 GTH e2, typeExp)  = case typeExp of
    Int  -> genCondition typeExp e1 e2 "if_icmpgt"
    Doub -> undefined
genExp (ERel e1 GE e2, typeExp)   = case typeExp of
    Int  -> genCondition typeExp e1 e2 "if_icmpge"
    Doub -> undefined
genExp (ERel e1 EQU e2, typeExp)  = case typeExp of
    Int  -> genCondition typeExp e1 e2 "if_icmpeq"
    Doub -> undefined
genExp (ERel e1 NE e2, typeExp)   = case typeExp of
    Int  -> genCondition typeExp e1 e2 "if_icmpne"
    Doub -> undefined

genExp (EAnd e1 e2, typeExp)      = undefined
genExp (EOr e1 e2, typeExp)       = undefined

genCondition :: Type -> Expr -> Expr -> String -> GenState ()
genCondition t e1 e2 s = do
    genExp (e1, t)
    genExp (e2, t)
    returnCode s

