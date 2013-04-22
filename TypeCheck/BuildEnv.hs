module BuildEnv where

import Context

import AbsJavalette
import PrintJavalette
import ErrM

addIOFun :: Env -> Env
addIOFun env = extendFun 
                (extendFun 
                    (extendFun 
                        (extendFun env readInt) 
                        printInt)
                    readDouble)
                printDouble

readInt :: TopDef
readInt  = FnDef Int  (Ident "readInt")  [] (Block [])
printInt :: TopDef
printInt = FnDef Void (Ident "printInt") [Arg Int (Ident "arg")] (Block [])
readDouble :: TopDef
readDouble  = FnDef Doub  (Ident "readDouble")  [] (Block [])
printDouble :: TopDef
printDouble = FnDef Void (Ident "printDouble") [Arg Doub (Ident "arg")] (Block [])


-- Update an env with a new function
extendFun :: Env -> TopDef -> Env 
extendFun env def = addSig signature env
    where
        (signature, _) = funToSign def
        addSig sig (s, c, sigs) = (s, c, sigs ++ [sig])
