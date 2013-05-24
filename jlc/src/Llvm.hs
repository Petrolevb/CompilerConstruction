module Llvm (generationLlvm) where

import System.Directory (removeFile, doesFileExist)
import Control.Monad.State

import AnnotatedAbs as TYP
import AbsJavalette as ABS

import LlvmContext

type GenState a = StateT LlvmContext IO a 


{-
        Integer : add, sub, mul, sdiv, srem
        Double  : fadd, fsub, fmul, fdiv
        Memory  : alloca, load, getelementptr, store
        Other   : icmp, fcmp, call
        cmp     : <  -> slt
                  >  -> sgt
                  >= -> sge
                  <= -> sle
                  == -> eq
                  != -> ne
-}

returnCode :: String -> GenState ()
returnCode s = do
    env <- get
    let fileName = getFileName env
    liftIO $ appendFile fileName s

getLetterFromType :: Type -> String
getLetterFromType t = case t of
    Int  -> "i32"
    Doub -> "double"
    Void -> "void"
    Bool -> "i1"
    Str  -> "i8"

extractFileName :: String -> String
extractFileName s = init (init s) ++ "ll"

generationLlvm :: AnnotatedProgram -> String -> IO ()
generationLlvm (AnnotatedProgram topdefs) s = do
    let fileName = extractFileName s
    ex <- doesFileExist fileName
    if ex 
        then do 
            removeFile fileName
            evalStateT (genProg topdefs) (newContext fileName)
        else evalStateT (genProg topdefs) (newContext fileName)

genProg :: [AnnotatedTopDef] -> GenState ()
genProg topdefs = do
    returnCode "declare void @printInt(i32)\n"
    returnCode "declare void @printDouble(double)\n"
    returnCode "declare void @printString(i8*)\n"
    returnCode "declare i32 @readInt()\n"
    returnCode "declare double @readDouble()\n\n"
    mapM_  genTopDef topdefs

genTopDef :: AnnotatedTopDef -> GenState ()
genTopDef (TYP.FnDef typeFn (Ident ident) args block) = do
    env <- get
    put $ addArgs (addFunc env (Ident ident)) args
    env <- get
    returnCode $ "define " ++ (getLetterFromType typeFn) ++ "@" ++ ident ++ "(" ++ (genArgs args) ++ ") {\n"
    returnCode $ "entry:\n"
    genBlock block
    returnCode $ "}\n\n"
        where 
            genArgs [Arg t (Ident s)]      = (getLetterFromType t) ++ "%" ++ s
            genArgs ((Arg t (Ident s)):as) = (getLetterFromType t) ++ "%" ++ s ++ ", " ++ (genArgs as)

genBlock :: AnnotatedBlock -> GenState ()
genBlock (AnnotatedBlock stmts) = mapM_ genStmt stmts

genStmt :: AnnotatedStmt -> GenState ()
genStmt TYP.Empty                 = returnCode ""
genStmt (TYP.BStmt block)         = genBlock block
genStmt (TYP.Decl typeDecl items) = undefined

genStmt (TYP.Ass ident exp)       = do
    env <- get
    let typeStr = getLetterFromType $ getType exp
    returnCode $ (getMemory env ident) ++ " = alloca " ++ typeStr ++ "\n"
    returnCode $ "store " ++ typeStr ++ " "
    genExp exp
    returnCode "\n"
genStmt (TYP.Incr ident)          = undefined
genStmt (TYP.Decr ident)          = undefined
genStmt (TYP.Ret exp)             = do
    returnCode $ "ret " ++ (getLetterFromType $ getType exp) ++ " "
    genExp exp
    returnCode "\n"
genStmt TYP.VRet                  = returnCode "ret\n"
genStmt (TYP.Cond (TYP.ELitTrue _) stmt)  = genStmt stmt
genStmt (TYP.Cond (TYP.ELitFalse _) stmt) = returnCode ""
genStmt (TYP.Cond (TYP.EOr e1 e2 t) stmt)       = undefined
genStmt (TYP.Cond exp stmt)                     = undefined
genStmt (TYP.CondElse exp s1 TYP.Empty)         = fail "Error : Empty if-else block statement"
genStmt (TYP.CondElse (TYP.ELitTrue _) s _)     = genStmt s
genStmt (TYP.CondElse (TYP.ELitFalse _) _ s)    = genStmt s
genStmt (TYP.CondElse (TYP.EOr e1 e2 t) s1 s2)  = undefined
genStmt (TYP.CondElse exp s1 s2)                = undefined
genStmt (TYP.While (TYP.EOr e1 e2 t) s1)        = undefined
genStmt (TYP.While exp stmt)                    = undefined
genStmt (TYP.SExp exp)                          = genExp exp




genExp :: AnnotatedExp -> GenState ()
genExp (TYP.EVar ident typeExp)        = undefined
genExp (TYP.ELitInt int _)             = returnCode $ show int 
genExp (TYP.ELitDoub double _)         = returnCode $ show double
genExp (TYP.ELitTrue _)                = returnCode $ "iconst_1" ++ "\n"
genExp (TYP.ELitFalse _)               = returnCode $ "iconst_0" ++" \n"
genExp (TYP.EApp (Ident s) exprs typeExp)  = do
    let var = ""
    case s of 
        "printInt"      -> returnCode $ "call void @printInt(i32 " ++ var ++ ")"
        "printDouble"   -> returnCode $ "call void @printInt(double " ++ var ++ ")"
        "readInt"       -> returnCode "call i32 @printInt()"
        "readDouble"    -> returnCode "call double @printInt()"
        "printString"   -> returnCode "invokestatic Runtime/printString(Ljava/lang/String;)V\n"
        _               -> undefined
genExp (TYP.EString string _)          = returnCode $ "ldc \"" ++ string ++ "\"\n"
genExp (TYP.Neg expr _ )               = undefined
genExp (TYP.Not expr typeExp)          = returnCode "Not\n"
genExp (TYP.EMul e1 Times e2 typeExp)  = undefined
genExp (TYP.EMul e1 Div e2 typeExp)    = undefined
genExp (TYP.EMul e1 Mod e2 typeExp)    = undefined
genExp (TYP.EAdd e1 Plus e2 typeExp)   = undefined
genExp (TYP.EAdd e1 Minus e2 typeExp)  = undefined
genExp (TYP.ERel e1 LTH e2 t)  = undefined
genExp (TYP.ERel e1 LE e2 t)   = undefined 
genExp (TYP.ERel e1 GTH e2 t)  = undefined 
genExp (TYP.ERel e1 GE e2 t)   = undefined 
genExp (TYP.ERel e1 EQU e2 t)  = undefined 
genExp (TYP.ERel e1 NE e2 t)   = undefined 
genExp (TYP.EAnd e1 e2 typeExp) = undefined
genExp (TYP.EOr e1 e2 typeExp)  = undefined
genExpOr (TYP.ERel e1 th e2 t) = undefined

genConditionDouble :: AnnotatedExp -> AnnotatedExp -> String -> String -> GenState ()
genConditionDouble e1 e2 s1 s2= undefined

genConditionInt :: AnnotatedExp -> AnnotatedExp -> String -> GenState ()
genConditionInt e1 e2 s = undefined

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

getComplexity :: AnnotatedExp -> Int
getComplexity (TYP.EVar     _ _) = 1
getComplexity (TYP.ELitInt  _ _) = 1
getComplexity (TYP.ELitDoub _ _) = 1
getComplexity (TYP.ELitTrue _)   = 1
getComplexity (TYP.ELitFalse _)  = 1
getComplexity (TYP.EString _ _)  = 1
getComplexity (TYP.Neg e _)      = getComplexity e
getComplexity (TYP.Not e _)      = getComplexity e
getComplexity _                  = 2

