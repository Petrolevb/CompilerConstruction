module GeneratorContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, CounterLabel, [Variables, memory], [StackLabel], FileName)
type GenContext = (Ident, Integer, MapVars, [String], FilePath)
type MapVars = [(Type, Ident, Int)]

newMap :: MapVars
newMap = []

newContext :: String -> GenContext
newContext fileName = ((Ident ""), 0, newMap, [], fileName)

getFileName :: GenContext -> FilePath
getFileName (_, _, _, _, fn) = fn

getMemory :: GenContext -> Ident -> (Type, Int)
getMemory (_, _, mv, _, _) = getVar mv

getVar :: MapVars -> Ident -> (Type, Int)
getVar ((t, id, i):maps) search | id == search = (t, i-1)
                                | otherwise = getVar maps search

addVar :: GenContext -> (Type, Ident) -> GenContext
addVar (id, c, mv, st, fn) (typ, ident) = (id, c, (addInMv mv typ ident), st, fn)
    where addInMv mv t i = mv ++ [(t, i, (length mv))]

getNameFunc :: GenContext -> String
getNameFunc ((Ident func), _, _, _, _) = func


getLabel :: GenContext -> String
getLabel ((Ident f), c, _, _, _) = f ++ "_" ++ show c
-- get a label with the name of the current function and a counter
incrLabel :: GenContext -> GenContext
incrLabel (f, c, mv, st, fn) = (f, c+1, mv, st, fn)

-- add a label on the stack of label
pushLabel :: GenContext -> String -> GenContext
pushLabel (id, c, mv, st, fn) label = (id, c, mv, (label:st), fn)

-- take the first label on the stack
stackLabel :: GenContext -> String
stackLabel (_, _, _, (label:st), _) = label
stackLabel (_, _, _, [], _)         = ""

-- withdraw the top label from the stack
popLabel :: GenContext -> GenContext
popLabel(id, c, mv, []    , fn) = (id, c, mv, [], fn)
popLabel(id, c, mv, (l:st), fn) = (id, c, mv, st, fn)



addFunc :: GenContext -> Ident -> GenContext
addFunc (_, c, mv, st, fn)  func = (func, c, mv, st, fn)

addArgs :: GenContext -> [Arg] -> GenContext
addArgs (f, c, mv, st, fn) args = (f, c, (mapArgs mv 1 args), st, fn)

mapArgs :: MapVars -> Int -> [Arg] -> MapVars
mapArgs mv _ [] = mv
mapArgs mv i ((Arg typeA ident):args) = mapArgs (mv ++ [(typeA, ident, i)]) (i+1) args

