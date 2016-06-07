module OIDefs where

import Data.List

type Name = String

data OIExpr =
   ILit Int
 | BLit Bool
 | Con Name
 | Var Name
 | Lam Name OIExpr
 | App OIExpr OIExpr
 | Let Name OIExpr OIExpr
 | LetA Name OIType OIExpr OIExpr
 | Case OIExpr [(OIPat, OIExpr)]
 | LetD Name [TypeVar] [DCons] OIExpr
 deriving (Eq, Show)

type DCons = (Name, [TypeVar], [OIConstraint], [OIType])
--            tname   bs          cons          arguments (tau_i)

-- only simple patterns: case x of [K x1 x2 ...]
-- no nested, no _, no "otherwise"
data OIPat = PCon Name [Name]
 deriving (Eq, Show)

type TypeVar = Int
type MetaVar = Int

data OIType =
   TInt
 | TBool
 | TFun OIType OIType
 | TCons Name [OIType]
 | TVar TypeVar
 | TMeta MetaVar
 | TForall [TypeVar] [OIConstraint] OIType
 deriving (Eq)

data OIConstraint =
   CEq OIType OIType                                       -- t1 ~ t2
 | CImp [MetaVar] [TypeVar] [OIConstraint] [OIConstraint]  -- [alphas] \forall betas Cs ⊃ Fs
 deriving (Eq)

fuv :: OIType -> [MetaVar]
fuv (TFun t1 t2) = (fuv t1) ++ (fuv t2)
fuv (TCons _ ts) = concat (map fuv ts)
fuv (TForall _ _ t) = fuv t
fuv (TMeta m) = [m]
fuv _ = []

ftv :: OIType -> [MetaVar]
ftv (TFun t1 t2) = (ftv t1) ++ (ftv t2)
ftv (TCons _ ts) = concat (map ftv ts)
ftv (TForall _ _ t) = ftv t
ftv (TVar v) = [v]
ftv _ = []

instance Show OIConstraint where
 show (CEq t1 t2) = (show t1) ++ " ~ " ++ (show t2)
 show (CImp metas tvars cs fs) = (show metas) ++ "∀"
   ++ (show tvars) ++ "." ++ (intercalate ", " $ map show cs)
   ++ " ⊃ " ++ (intercalate ", " $ map show fs)

_tvars = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
_metas = ["α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","ς","σ","τ","υ","φ","χ","ψ"]

instance Show OIType where
 show (TInt) = "I"
 show (TBool) = "B"
 show (TFun t1 t2) = (show t1) ++ "->" ++ (show t2)
 show (TCons n ts) = n ++ (intercalate " " $ map show ts)
 show (TVar v) = let v' = v-100 in if 0 <= v' && v' < length(_tvars) then _tvars !! v' else (show v)
 show (TMeta m) = if 0 <= m && m < length(_metas) then _metas !! m else (show m)
 show (TForall tvars cs t) = "∀" ++ (show tvars) ++ "." ++ (intercalate ", " $ map show cs) ++ " ⇒ " ++ (show t)
