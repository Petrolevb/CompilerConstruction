module GeneratorContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, CounterLabel, [Variables, memory]
type GenContext = (Ident, Integer, MapVars)
type MapVars = [(Type, Ident, Integer)]

newMap :: MapVars
newMap = []

newContext :: GenContext
newContext = ((Ident ""), 0, newMap)

getMemory :: GenContext -> Ident -> (Type, Integer)
getMemory (_, _, mv) = getVar mv

getVar :: MapVars -> Ident -> (Type, Integer)
getVar ((t, id, i):maps) search | id == search = (t, i)
                                | otherwise = getVar maps search

getNameFunc :: GenContext -> String
getNameFunc ((Ident func), _, _) = func


getLabel :: GenContext -> String
getLabel ((Ident f), c, _) = f ++ "_" ++ show c
-- get a label with the name of the current function and a counter
incrLabel :: GenContext -> GenContext
incrLabel (f, c, mv) = (f, c+1, mv)



addFunc :: GenContext -> Ident -> GenContext
addFunc (_, c, mv)  func = (func, c, mv)

addArgs :: GenContext -> [Arg] -> GenContext
addArgs (f, c, mv) args = (f, c, (mapArgs mv 1 args))

mapArgs :: MapVars -> Integer -> [Arg] -> MapVars
mapArgs mv _ [] = mv
mapArgs mv i ((Arg typeA ident):args) = mapArgs (mv ++ [(typeA, ident, i)]) (i+1) args


