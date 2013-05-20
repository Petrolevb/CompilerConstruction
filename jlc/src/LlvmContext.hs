module LlvmContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, (Counter: Label, Variable), [Javalette_var, Llvm_name], [name, content], [StackLabel])
type LlvmContext = (Ident, (Int, Int), MapVars, MapStrs, [String])
type MapVars = [(Ident, String)]
type MapStrs = [(String, String)]

newMapVars :: MapVars
newMapVars = []

newMapStrs :: MapStrs
newMapStrs = []

newContext :: LlvmContext
newContext = (Ident "", (0, 0), newMapVars, newMapStrs, [])


getMemory :: LlvmContext -> Ident -> String
getMemory (_, _, mv, _, _) = getVar mv

getVar :: MapVars -> Ident -> String
getVar mv id = case lookup id mv of
                    Just x -> x
                    _      -> fail "Variable not found"

-- Add the javalette ident to the lookup table with a new name
addVar :: LlvmContext -> Ident -> LlvmContext
addVar (id, (cl,cv), mv, ms, st) (Ident i) = (id, (cl, cv+1), addInMv mv i cv, ms, st)
    where addInMv mv i cv = mv ++ [(Ident i, i ++ show cv)]


getNameFunc :: LlvmContext -> String
getNameFunc (Ident func, _, _, _, _) = func

addFunc :: LlvmContext -> Ident -> LlvmContext
addFunc (_, c, mv, ms, st)  func = (func, c, mv, ms, st)

addArgs :: LlvmContext -> [Arg] -> LlvmContext
addArgs (f, (cl, cv), mv, ms, st) args = (f, (cl, cv + length args), mapArgs mv (cl, cv) args, ms, st)

mapArgs :: MapVars -> (Int, Int) -> [Arg] -> MapVars
mapArgs mv _ [] = mv
mapArgs mv (cl, cv) (Arg typeA (Ident i):args) = mapArgs (mv ++ [(Ident i, i++ show cv)]) (cl, cv+1) args


getLabel :: LlvmContext -> String
getLabel (Ident f, (cl, cv), _, _, _) = f ++ "_" ++ show cl
-- get a label with the name of the current function and a counter
incrLabel :: LlvmContext -> LlvmContext
incrLabel (f, (cl, cv), mv, ms, st) = (f, (cl+1, cv), mv, ms, st)

-- add a label on the stack of label
pushLabel :: LlvmContext -> String -> LlvmContext
pushLabel (id, c, mv, ms, st) label = (id, c, mv, ms, label:st)

-- take the first label on the stack
stackLabel :: LlvmContext -> String
stackLabel (_, _, _, _, label:st) = label
stackLabel (_, _, _, _, [])         = ""

-- withdraw the top label from the stack
popLabel :: LlvmContext -> LlvmContext
popLabel(id, c, mv, ms, l:st) = (id, c, mv, ms, st)
