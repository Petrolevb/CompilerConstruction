module GeneratorContext where

import AnnotatedAbs as TYP
import AbsJavalette as ABS

type GenContext = String

getMemory :: GenContext -> Ident -> Integer
getMemory = undefined

getNameFunc :: GenContext -> String
getNameFunc = undefined


getLabel :: GenContext -> String
getLabel = undefined
-- get a label with the name of the current function and a counter
