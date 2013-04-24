module GeneratorContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, CounterLabel, [Variables, memory]
type GenContext = (Ident, Integer, MapVars)
type MapVars = [(Ident, Integer)]

newMap :: MapVars
newMap = []

newContext :: GenContext
newContext = ((Ident ""), 0, newMap)

getMemory :: GenContext -> Ident -> Integer
getMemory (_, _, mv)= undefined

getNameFunc :: GenContext -> String
getNameFunc ((Ident func), _, _) = func


getLabel :: GenContext -> String
getLabel ((Ident f), c, _) = f ++ "_" ++ show c
-- get a label with the name of the current function and a counter
incrLabelEnv :: GenContext -> GenContext
incrLabelEnv (f, c, mv) = (f, c+1, mv)

addFunc :: GenContext -> Ident -> GenContext
addFunc (_, c, mv)  func = (func, c, mv)
