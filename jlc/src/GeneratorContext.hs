module GeneratorContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, CounterLabel, [Variables, memory], [StackLabel])
type GenContext = (Ident, Integer, MapVars, [String])
type MapVars = [(Type, Ident, Int)]

newMap :: MapVars
newMap = []

newContext :: GenContext
newContext = ((Ident ""), 0, newMap, [])

getMemory :: GenContext -> Ident -> (Type, Int)
getMemory (_, _, mv, _) = getVar mv

getVar :: MapVars -> Ident -> (Type, Int)
getVar ((t, id, i):maps) search | id == search = (t, i)
                                | otherwise = getVar maps search

addVar :: GenContext -> (Type, Ident) -> GenContext
addVar (id, c, mv, st) (typ, ident) = (id, c, (addInMv mv typ ident), st)
    where addInMv mv t i = mv ++ [(t, i, (length mv))]

getNameFunc :: GenContext -> String
getNameFunc ((Ident func), _, _, _) = func


getLabel :: GenContext -> String
getLabel ((Ident f), c, _, _) = f ++ "_" ++ show c
-- get a label with the name of the current function and a counter
incrLabel :: GenContext -> GenContext
incrLabel (f, c, mv, st) = (f, c+1, mv, st)

-- add a label on the stack of label
pushLabel :: GenContext -> String -> GenContext
pushLabel (id, c, mv, st) label = (id, c, mv, (label:st))

-- take the first label on the stack
stackLabel :: GenContext -> String
stackLabel (_, _, _, (label:st)) = label
stackLabel (_, _, _, [])         = ""

-- withdraw the top label from the stack
popLabel :: GenContext -> GenContext
popLabel(id, c, mv, (l:st)) = (id, c, mv, st)



addFunc :: GenContext -> Ident -> GenContext
addFunc (_, c, mv, st)  func = (func, c, mv, st)

addArgs :: GenContext -> [Arg] -> GenContext
addArgs (f, c, mv, st) args = (f, c, (mapArgs mv 1 args), st)

mapArgs :: MapVars -> Int -> [Arg] -> MapVars
mapArgs mv _ [] = mv
mapArgs mv i ((Arg typeA ident):args) = mapArgs (mv ++ [(typeA, ident, i)]) (i+1) args

