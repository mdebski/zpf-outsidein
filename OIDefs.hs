module OIDefs where

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

-- only simple patterns: case x of [K x1 x2 ...]
-- no nested, no _, no "other"
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
 deriving (Eq, Show)

data OIConstraint =
   CEq OIType OIType                                       -- t1 ~ t2
 | CImp [MetaVar] [TypeVar] [OIConstraint] [OIConstraint]  -- [alphas] \forall betas Cs ⊃ Fs
 deriving (Eq, Show)
