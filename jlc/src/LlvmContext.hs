module LlvmContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, CounterLabel, [Javalette_var, Llvm_name], [name, content], [StackLabel])
type LlvmContext = (Ident, Integer, MapVars, MapStrs, [String])
type MapVars = [(Ident, String)]
type MapStrs = [(String, String)]

newMapVars :: MapVars
newMapVars = []

newMapStrs :: MapStrs
newMapStrs = []

newContext :: LlvmContext
newContext = ((Ident ""), 0, newMapVars, newMapStrs, [])


getMemory :: LlvmContext -> Ident -> String
getMemory (_, _, mv, _) = getVar mv

getVar :: MapVars -> Ident -> (Type, Int)
getVar mv id = case lookup id mv of
                    Just x -> Just x
                    _      -> fail "Variable not found"

-- Add the javalette ident to the lookup table with a new name
addVar :: LlvmContext -> Ident -> LlvmContext
addVar (id, c, mv, st) (Ident i) = (id, c+1, (addInMv mv i c), st)
    where addInMv mv i c = mv ++ [(Ident i, (show i)++c)]


getNameFunc :: LlvmContext -> String
getNameFunc ((Ident func), _, _, _) = func

addFunc :: LlvmContext -> Ident -> LlvmContext
addFunc (_, c, mv, st)  func = (func, c, mv, st)

addArgs :: LlvmContext -> [Arg] -> LlvmContext
addArgs (f, c, mv, st) args = (f, c+(length args), (mapArgs mv c args), st)

mapArgs :: MapVars -> Int -> [Arg] -> MapVars
mapArgs mv _ [] = mv
mapArgs mv c (Arg typeA (Ident i):args) = mapArgs (mv ++ [(Ident i, (show i)++c)]) (c+1) args

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
