module OIMonad where

import Control.Monad
import qualified Data.Map as Map
import qualified Control.Exception.Base as Exc
import Control.Monad.Trans.State (StateT, put, get, runStateT)

import OIDefs
import Subs

data OIState = OIState {
 nextVar :: Int,
 nextMeta :: Int,
 envs :: [Map.Map Name OIType],
 dcons :: Map.Map Name ([TypeVar], [OIConstraint], [OIType]),
 tcons :: Map.Map Name ([TypeVar], [Name]),
 d2t :: Map.Map Name Name -- data constructor name to type constructor name
} deriving (Eq, Show)

getTCon :: Name -> OI ([TypeVar], [Name])
getTCon n = do
 OIState{tcons=tcons} <- get
 return $ tcons Map.! n
getDCon :: Name -> OI ([TypeVar], [OIConstraint], [OIType])
getDCon n = do
 OIState{dcons=dcons} <- get
 return $ dcons Map.! n

baseState :: OIState
baseState = OIState{
 nextVar = 100,
 nextMeta = 0,
 envs = [Map.empty],
 dcons = Map.empty,
 tcons = Map.empty,
 d2t = Map.empty
}

type OI = (StateT OIState) IO
runOI :: OI a -> IO (a, OIState)
runOI m = runStateT m baseState

freshVar :: OI TypeVar
freshVar = do
 state@OIState{nextVar=nextVar} <- get
 put state{nextVar = nextVar+1}
 return nextVar

freshMeta :: OI OIType
freshMeta = do
 state@OIState{nextMeta=nextMeta} <- get
 put state{nextMeta=nextMeta+1}
 return $ TMeta nextMeta

addData :: Name -> [TypeVar] -> [DCons] -> OI ()
addData tname tvars newDcons = do
 state@OIState{dcons=dcons, tcons=tcons, d2t=d2t} <- get
 let dcons' = Map.fromList [(dname, (tvs, cs, ts)) | (dname, tvs, cs, ts) <- newDcons]
 let dconsNames = Map.keys dcons'
 let d2t' = Map.fromList [(dname, tname) | dname <- dconsNames]
 put state{tcons=Map.insert tname (tvars, dconsNames) tcons,
           dcons=Map.union dcons dcons',
           d2t=Map.union d2t d2t'}

getType :: Name -> OI OIType
getType name = do
 OIState{envs=envs} <- get
 return $ (Map.unions envs) Map.! name

withTypes :: [Name] -> [OIType] -> OI a -> OI a
withTypes ns ts m = do
 state@OIState{envs=envs} <- get
 put state{envs=(Map.fromList (zip ns ts)):envs}
 res <- m
 put state{envs=envs}
 return res

withType :: Name -> OIType -> OI a -> OI a
withType n t = withTypes [n] [t]

assert :: Bool -> OI ()
assert b = void $ return $ Exc.assert b ()

getEnvFuvWithSub :: Sub -> OI [MetaVar]
getEnvFuvWithSub s = do
 OIState{envs=envs} <- get
 let env = Map.unions envs
 let ts = Map.elems env
 return $ concatMap fuv (map (applySub s) ts)

getEnvFuv :: OI [MetaVar]
getEnvFuv = getEnvFuvWithSub emptySub
