module OIMonad where

import Control.Monad.Trans.State (StateT, put, get)

import OIDefs

data OIState = OIState {
 nextVar :: Int,
 nextMeta :: Int
} deriving (Eq, Show)

baseState :: OIState
baseState = OIState{
 nextVar = 100,
 nextMeta = 0
}

type OI = (StateT OIState) IO

freshVar :: OI TypeVar
freshVar = do
 state@OIState{nextVar=nextVar} <- get
 put state{nextVar = nextVar+1}
 return nextVar

freshMeta :: OI MetaVar
freshMeta= do
 state@OIState{nextMeta=nextMeta} <- get
 put state{nextMeta = nextMeta+1}
 return nextMeta
