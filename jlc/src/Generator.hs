module Generator(generationJvm) where

import System.Directory (removeFile, doesFileExist)
import Control.Monad.State

import AnnotatedAbs as TYP
import AbsJavalette as ABS

import GeneratorContext
import Size

type GenState a = StateT GenContext IO a 

returnCode :: String -> GenState ()
returnCode s = do
    env <- get
    let fileName = getFileName env
    liftIO $ appendFile (fileName ++ ".j") s


getLetterFromType :: Type -> Char
getLetterFromType t = case t of
    Int  -> 'I'
    Doub -> 'D'
    Void -> 'V'
    Bool -> 'I'
    Str  -> 'S'

getLettersArgs :: [Arg] -> String
getLettersArgs  = map (\(Arg typeA _) -> getLetterFromType typeA)

getLettersExps :: [AnnotatedExp] -> String
getLettersExps = map (getLetterFromType.getType)

generationJvm :: AnnotatedProgram -> String -> IO ()
generationJvm (AnnotatedProgram topdefs) s = do
    let fileName = init $ init $ init s
    ex <-  doesFileExist fileName 
    if ex 
        then do 
            removeFile (fileName ++ ".j")
            evalStateT (genProg topdefs) (newContext fileName)
        else evalStateT (genProg topdefs) (newContext fileName)

genProg :: [AnnotatedTopDef] -> GenState ()
genProg topdefs = do
    env <- get
    let fileName = getFileName env
    returnCode $ ".class public " ++ fileName ++ "\n"
    returnCode ".super java/lang/Object\n\n"
    returnCode ".method public <init>()V\naload_0\ninvokespecial java/lang/Object/<init>()V\nreturn\n.end method\n\n"
    returnCode ".method public static main([Ljava/lang/String;)V\n"
    returnCode ".limit locals 1\n"
    returnCode $ "invokestatic " ++ fileName ++ "/main()V\n"
    returnCode "pop\nreturn\n.end method\n\n"
    mapM_  genTopDef topdefs

genTopDef :: AnnotatedTopDef -> GenState ()
genTopDef (TYP.FnDef typeFn ident args block) = do
    env <- get
    put $ addArgs (addFunc env ident) args
    env <- get
    let (local, stack) = getLocalaStackSize block
    returnCode $ ".method public static " ++ getNameFunc env
    returnCode $ "(" ++ getLettersArgs args ++ ")" ++ getLetterFromType typeFn : "\n"
    returnCode $ ".limit locals " ++ show local ++ "\n"
    returnCode $ ".limit stack " ++ show stack ++ "\nentry:\n"
    genBlock block
    returnCode ".end method\n\n"

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
        Doub -> returnCode $ "dstore " ++ show pos ++ "\n"
        _    -> returnCode $ "istore " ++ show pos ++ "\n"

genStmt (TYP.Incr ident)          = do
    env <- get
    returnCode $  "iinc " ++  show (snd (getMemory env ident)) ++ " 1\n"
genStmt (TYP.Decr ident)          = do
    env <- get
    returnCode $ "iinc " ++  show (snd (getMemory env ident)) ++ " -1\n"
genStmt (TYP.Ret exp)             = do
    genExp exp
    case getType exp of
        Int  -> returnCode "ireturn\n"
        Bool -> returnCode "ireturn\n"
        Doub -> returnCode "dreturn\n"     
genStmt TYP.VRet                  = returnCode "ireturn\n"

genStmt (TYP.Cond (TYP.ELitTrue _) stmt)  = genStmt stmt
genStmt (TYP.Cond (TYP.ELitFalse _) stmt) = returnCode ""
genStmt (TYP.Cond cond              stmt) = do
    env <- get
    let lab1 = getLabel env
    let newEnv = incrLabel env
    put $ pushLabel newEnv lab1    
    genExp cond
    genStmt stmt
    returnCode $ lab1 ++ ":\n"

genStmt (TYP.CondElse exp s1 TYP.Empty)  = do
    env <- get
    fail $ "Empty branch exist in " ++ getNameFunc env ++ " in a if else statement"
genStmt (TYP.CondElse (TYP.ELitTrue _) s _)  = genStmt s
genStmt (TYP.CondElse (TYP.ELitFalse _) _ s) = genStmt s
genStmt (TYP.CondElse cond s1 s2) = do
    addLabels
    env <- get
    let lab1 = stackLabel env
    let newEnv = popLabel env
    let lab2 = stackLabel newEnv

    case cond of
         TYP.EOr e1 e2 t -> do
                 switchTopStack
                 genExp (TYP.EOr e1 e2 t)
                 returnCode $ lab2 ++ ":\n"
         _                   -> do
                  genExp cond
                  env <- get
                  put $ popLabel env

    genStmt s1
    if not containReturn
      then do
          env <- get
          let lab3 = getLabel env
          let newEnv = incrLabel env
          put $ pushLabel newEnv lab3
          returnCode $ "goto " ++ lab3 ++ "\n"
      else return ()
    returnCode $ lab1 ++ ":\n"
    genStmt s2
    if not containReturn
      then do
          env <- get
          let lab3 = stackLabel env
          put $ popLabel env
          returnCode $ lab3 ++ ":\n"
      else return ()
    env <- get
    put $ popLabel env
    where containReturn = isReturn s1

genStmt (TYP.While cond stmt) = do
    addLabels
    env <- get
    let lab1 = stackLabel env
    let newEnv = popLabel env
    let lab2 = stackLabel newEnv
    returnCode $ lab1 ++ ":\n"
    
    case cond of
         TYP.EOr e1 e2 t -> do
                 env <- get
                 let lab3 = getLabel env
                 let tmpEnv = incrLabel env
                 put $ pushLabel tmpEnv lab3
                 genExp (TYP.EOr e1 e2 t)
                 returnCode $ lab3 ++ ":\n"
                 env <- get
                 let newEnv = popLabel env
                 put $ popLabel newEnv
         _ -> do
                  env <- get
                  put $ popLabel env
                  genExp cond
    genStmt stmt
    returnCode $ "goto " ++ lab1 ++ "\n"
    returnCode $ lab2 ++ ":\n"
    env <- get
    put $ popLabel env

genStmt (TYP.SExp exp) = genExp exp


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
genExp (TYP.EApp (Ident s) exprs typeExp)  = do
    mapM_ genExp exprs
    case s of 
        "printInt"      -> returnCode "invokestatic Runtime/printInt(I)V\n"
        "printDouble"   -> returnCode "invokestatic Runtime/printDouble(D)V\n"
        "readInt"       -> returnCode "invokestatic Runtime/readInt()I\n"
        "readDouble"    -> returnCode "invokestatic Runtime/readDouble()D\n"
        "printString"   -> returnCode "invokestatic Runtime/printString(Ljava/lang/String;)V\n"
        _               -> returnCode $ "invokestatic " ++ s ++ "(" ++ (getLettersExps exprs) ++ ")"
                                        ++ (getLetterFromType typeExp):"\n"
genExp (TYP.EString string _)          = returnCode $ "ldc \"" ++ string ++ "\"\n"
genExp (TYP.Neg expr _ )               = do
    genExp expr
    returnCode "ineg\n"

genExp (TYP.Not (TYP.ERel e1 th e2 t) typeExp) = genExpOr (TYP.ERel e1 th e2 t)
genExp (TYP.Not expr typeExp)      = do
    env <- get
    let lab = stackLabel env
    addLabels
    env <- get
    let lab1 = stackLabel env
    let tmpEnv = popLabel env
    let lab2 = stackLabel tmpEnv
    genExp expr
    env <- get
    let tmpEnv = popLabel env
    put $ popLabel tmpEnv
    returnCode $ lab1 ++ ":\n"
    returnCode $ "goto " ++ lab ++ "\n"
    returnCode $ lab2 ++ ":\n"

genExp (TYP.EMul e1 op e2 typeExp) = do
   genExp e1
   genExp e2
   case typeExp of
        Int -> returnCode $ 'i' : tOp
        Doub-> returnCode $ 'd' : tOp
   where tOp = case op of
                    Times -> "mul\n"
                    Div   -> "div\n"
                    Mod   -> "rem\n"

genExp (TYP.EAdd e1 op e2 typeExp) = do
   genExp e1
   genExp e2
   case typeExp of
        Int -> returnCode $ 'i' : tOp
        Doub-> returnCode $ 'd' : tOp
   where tOp = case op of
                    Plus  -> "add\n"
                    Minus -> "sub\n"

genExp (TYP.EAnd e1 e2 typeExp) = do
       genExp e1
       genExp e2

genExp (TYP.EOr e1 e2 typeExp)  = do
       genExpOr e1
       env <- get
       put $ popLabel env
       genExp e2

genExp (TYP.ERel e1 th e2 t) = do
    let typeExp = getType e1
    case typeExp of
        Int  -> genConditionInt e1 e2 ("if_icmp" ++ cond)
        Doub -> genConditionDouble e1 e2 "dcmpl" ("if" ++ cond)
  where cond = case th of
                  LTH -> "ge"
                  LE  -> "gt"
                  GTH -> "le"
                  GE  -> "lt"
                  EQU -> "ne"
                  NE  -> "eq"


genExpOr :: AnnotatedExp -> GenState ()
genExpOr (TYP.ERel e1 th e2 t) = do
    let typeExp = getType e1
    case typeExp of
        Int  -> genConditionInt e1 e2 ("if_icmp" ++ cond)
        Doub -> genConditionDouble e1 e2 "dcmpl" ("if" ++ cond)
  where cond = case th of
                  LTH -> "lt"
                  LE  -> "le"
                  GTH -> "gt"
                  GE  -> "ge"
                  EQU -> "eq"
                  NE  -> "ne"
genExpOr exp = genExp exp


genConditionDouble :: AnnotatedExp -> AnnotatedExp -> String -> String -> GenState ()
genConditionDouble e1 e2 s1 s2= do
    genExp e1
    genExp e2
    env <- get
    let label = stackLabel env
    returnCode $ s1 ++ "\n" ++ s2 ++ " " ++ label ++ "\n"

genConditionInt :: AnnotatedExp -> AnnotatedExp -> String -> GenState ()
genConditionInt e1 e2 s = do
    genExp e1
    genExp e2
    env <- get
    let label = stackLabel env
    returnCode $ s ++ " " ++ label ++ "\n"


addLabels :: GenState ()
addLabels = do
    env <- get
    let lab1 = getLabel env
    let tmpEnv = incrLabel env
    let lab2 = getLabel tmpEnv
    let tmpEnv1 = incrLabel tmpEnv
    let tmpEnv2 = pushLabel tmpEnv1 lab2
    put $ pushLabel tmpEnv2 lab1

switchTopStack :: GenState ()
switchTopStack = do
    env <- get
    let lab1 = stackLabel env
    let tmpEnv = popLabel env
    let lab2 = stackLabel env
    let tmpEnv2 = popLabel env
    let newEnv = pushLabel tmpEnv2 lab1
    put $ pushLabel newEnv lab2


genDecl :: Type -> [AnnotatedItem] -> GenState ()
genDecl t = mapM_  (genItem t)
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


isReturn :: TYP.AnnotatedStmt -> Bool
isReturn (TYP.BStmt (AnnotatedBlock block)) = or $ map isReturn block
isReturn (TYP.Ret _)            = True
isReturn  TYP.VRet              = True 
isReturn (TYP.Cond _ s)         = False || isReturn s
isReturn (TYP.CondElse _ s1 s2) = False || isReturn s1 || isReturn s2
isReturn (TYP.While _ s)        = False || isReturn s
isReturn _                      = False
