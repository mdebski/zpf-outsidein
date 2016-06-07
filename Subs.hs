module Subs where

import qualified Data.Map as Map
import qualified Data.Set as Set

import OIDefs

data SubVar = SVar TypeVar | SMeta MetaVar deriving (Eq, Ord)
type Sub = Map.Map SubVar OIType

instance Show SubVar where
 show (SVar x) = show $ TVar x
 show (SMeta x) = show $ TMeta x

makeSub :: [SubVar] -> [OIType] -> Sub
makeSub ss ts = Map.fromList $ zip ss ts

emptySub = makeSub [] []

applySub :: Sub -> OIType -> OIType
applySub s (TFun t1 t2) = TFun (applySub s t1) (applySub s t2)
applySub s (TCons n ts) = TCons n (map (applySub s) ts)
applySub s (TForall a b t) = TForall a b (applySub s t)
applySub s ot@(TMeta m) = case Map.lookup (SMeta m) s of
 Just t -> t
 Nothing -> ot
applySub s ot@(TVar v) = case Map.lookup (SVar v) s of
 Just t -> t
 Nothing -> ot
applySub s t = t

applySubC :: Sub -> OIConstraint -> OIConstraint
applySubC s (CEq t1 t2) = CEq (applySub s t1) (applySub s t2)
applySubC s (CImp metas tvars cs1 cs2) = CImp metas' tvars cs1' cs2' where
 metas' = concatMap fuv (map (applySub s) (map TMeta metas))
 cs1' = (map (applySubC s) cs1)
 cs2' = (map (applySubC s) cs2)

compSub :: Sub -> Sub -> Sub
compSub s1 s2 = Map.union s2 (Map.map (applySub s2) s1)

domain :: Sub -> [SubVar]
domain = Map.keys

-- untouch = Set.fromList $ (map SMeta metas) ++ (map SVar tvars)
-- (s', _) = Map.partitionWithKey (\subvar -> \_ -> Set.member subvar untouch) s
