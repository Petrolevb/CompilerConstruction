module LlvmContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, CounterVariable, [Javalette_var, Llvm_name], [name, content], [StackLabel])
type LlvmContext = (Ident, Int, MapVars, MapStrs, [String])
type MapVars = [(Ident, String)]
type MapStrs = [(String, String)]

newMapVars :: MapVars
newMapVars = []

newMapStrs :: MapStrs
newMapStrs = []

newContext :: LlvmContext
newContext = ((Ident ""), 0, newMapVars, newMapStrs, [])


getMemory :: LlvmContext -> Ident -> String
getMemory (_, _, mv, _, _) = getVar mv

getVar :: MapVars -> Ident -> String
getVar mv id = case lookup id mv of
                    Just x -> x
                    _      -> fail "Variable not found"

-- Add the javalette ident to the lookup table with a new name
addVar :: LlvmContext -> Ident -> LlvmContext
addVar (id, c, mv, ms, st) (Ident i) = (id, c+1, (addInMv mv i c), ms, st)
    where addInMv mv i c = mv ++ [(Ident i, i ++ (show c))]


getNameFunc :: LlvmContext -> String
getNameFunc ((Ident func), _, _, _, _) = func

addFunc :: LlvmContext -> Ident -> LlvmContext
addFunc (_, c, mv, ms, st)  func = (func, c, mv, ms, st)

addArgs :: LlvmContext -> [Arg] -> LlvmContext
addArgs (f, c, mv, ms, st) args = (f, c+(length args), (mapArgs mv c args), ms, st)

mapArgs :: MapVars -> Int -> [Arg] -> MapVars
mapArgs mv _ [] = mv
mapArgs mv c (Arg typeA (Ident i):args) = mapArgs (mv ++ [(Ident i, i++ (show c))]) (c+1) args

{-
getLabel :: LlvmContext -> String
getLabel ((Ident f), c, _, _) = f ++ "_" ++ show c
-- get a label with the name of the current function and a counter
incrLabel :: LlvmContext -> LlvmContext
incrLabel (f, c, mv, st) = (f, c+1, mv, st)

-- add a label on the stack of label
pushLabel :: LlvmContext -> String -> LlvmContext
pushLabel (id, c, mv, st) label = (id, c, mv, (label:st))

-- take the first label on the stack
stackLabel :: LlvmContext -> String
stackLabel (_, _, _, (label:st)) = label
stackLabel (_, _, _, [])         = ""

-- withdraw the top label from the stack
popLabel :: LlvmContext -> LlvmContext
popLabel(id, c, mv, (l:st)) = (id, c, mv, st)
-}
