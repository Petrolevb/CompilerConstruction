module GeneratorContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS


-- Context = (NameCurrentFunction, CounterLabel, [Variables, memory], [StackLabel], FileName)
type GenContext = (Ident, Integer, MapVars, [String], FilePath)
type MapVars = [[(Type, Ident, Int)]]

newMap :: MapVars
newMap = [[]]

newContext :: String -> GenContext
newContext fileName = ((Ident ""), 0, newMap, [], fileName)

getFileName :: GenContext -> FilePath
getFileName (_, _, _, _, fn) = fn

getMemory :: GenContext -> Ident -> (Type, Int)
getMemory (_, _, mv, _, _) = getVar mv

getVar :: MapVars -> Ident -> (Type, Int)
getVar (map:stack) search  | snd $ lookAt map search = fst $ lookAt map search
                           | otherwise  = getVar stack search

lookAt :: [(Type, Ident, Int)] -> Ident -> ((Type, Int), Bool)
lookAt ((t, id, i):maps) search | id == search = ((t, i), True)
                                | otherwise    = lookAt maps search
lookAt [] _ = ((Void, 0), False)

addVar :: GenContext -> (Type, Ident) -> GenContext
addVar (id, c, mv, st, fn) (typ, ident) = (id, c, (addInMv mv typ ident), st, fn)
    where addInMv mv t i = ((t, i, sizeMv mv) : head mv) : drop 1 mv
          sizeMv mv = sum $ map length mv

stackVar :: GenContext -> GenContext
stackVar (id, c, mv, st, fn) = (id, c, []:mv, st, fn)

popVar :: GenContext -> GenContext
popVar (id, c, (m:mv), st, fn) = (id, c, mv, st, fn)


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
addArgs (f, c, mv, st, fn) args = (f, c, (mapArgs mv 0 args), st, fn)

mapArgs :: MapVars -> Int -> [Arg] -> MapVars
mapArgs mv _ [] = mv
mapArgs mv i ((Arg typeA ident):args) = mapArgs (addOnTop : (drop 1 mv)) (i+1) args
  where addOnTop = (head mv) ++ [(typeA, ident, i)]

newArgs :: GenContext -> GenContext
newArgs (f, c, mv, st, fn) = (f, c, newMap, st, fn)
