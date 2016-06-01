module OIMonad where

import Control.Monad
import qualified Data.Map as Map
import qualified Control.Exception.Base as Exc
import Control.Monad.Trans.State (StateT, put, get)


import OIDefs

data OIState = OIState {
 nextVar :: Int,
 nextMeta :: Int,
 env :: Map.Map Name OIType,
 fuv :: [[MetaVar]]
} deriving (Eq, Show)

baseState :: OIState
baseState = OIState{
 nextVar = 100,
 nextMeta = 0,
 env = Map.empty,
 fuv = [[]]
}

type OI = (StateT OIState) IO

freshVar :: OI OIType
freshVar = do
 state@OIState{nextVar=nextVar} <- get
 put state{nextVar = nextVar+1}
 return $ TVar nextVar

freshMeta :: OI OIType
freshMeta = do
 state@OIState{nextMeta=nextMeta,fuv=(f:fs)} <- get
 put state{nextMeta=nextMeta+1,fuv=((nextMeta:f):fs)}
 return $ TMeta nextMeta

getType :: Name -> OI OIType
getType name = do
 OIState{env=env} <- get
 return $ env Map.! name

withType :: Name -> OIType -> OI a -> OI a
withType n t m = do
 state@OIState{env=env, fuv=fuv} <- get
 put state{env=Map.insert n t env, fuv=[]:fuv}
 res <- m
 put state{env=env, fuv=fuv}
 return res

getFuv :: OI [MetaVar]
getFuv = do
 OIState{fuv=fuv} <- get
 return $ concat fuv

assert :: Bool -> OI ()
assert b = void $ return $ Exc.assert b ()
