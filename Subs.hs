module Subs where

import qualified Data.Map as Map

import OIDefs

data SubVar = SVar TypeVar | SMeta MetaVar
type Sub = Map.Map SubVar OIType

makeSub :: [SubVar] -> [OIType] -> Sub
makeSub = undefined

applySub :: Sub -> OIType -> OIType
applySub = undefined
