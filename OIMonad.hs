module OIMonad where

import Control.Monad
import qualified Data.Map as Map
import qualified Control.Exception.Base as Exc
import Control.Monad.Trans.State (StateT, put, get)


import OIDefs

data OIState = OIState {
 nextVar :: Int,
 nextMeta :: Int,
 env :: Map.Map Name OIType
} deriving (Eq, Show)

baseState :: OIState
baseState = OIState{
 nextVar = 100,
 nextMeta = 0,
 env = Map.empty
}

type OI = (StateT OIState) IO

freshVar :: OI OIType
freshVar = do
 state@OIState{nextVar=nextVar} <- get
 put state{nextVar = nextVar+1}
 return $ TVar nextVar

freshMeta :: OI OIType
freshMeta = do
 state@OIState{nextMeta=nextMeta} <- get
 put state{nextMeta = nextMeta+1}
 return $ TMeta nextMeta

getType :: Name -> OI OIType
getType name = do
 OIState{env=env} <- get
 return $ env Map.! name

withType :: Name -> OIType -> OI a -> OI a
withType n t m = do
 state@OIState{env=env} <- get
 put state{env=Map.insert n t env}
 res <- m
 put state{env=env}
 return res

assert :: Bool -> OI ()
assert b = void $ return $ Exc.assert b ()
