module OIDefs where

type VName = String
type TName = String

data OIExpr =
   ILit Int
 | BLit Bool
 | Con TName
 | Var VName
 | Lam VName OIExpr
 | App OIExpr OIExpr
 | Let VName OIExpr OIExpr
 | LetA VName OIType OIExpr OIExpr
 | Case OIExpr [(OIPat, OIExpr)]
 deriving (Eq, Show)

data OIPat =
   PVar VName
 | PCon TName [OIPat]
 deriving (Eq, Show)

type TypeVar = Int
type MetaVar = Int

data OIType =
   TInt
 | TBool
 | TFun OIType OIType
 | TCons TName [OIType]
 | TVar TypeVar
 | TMeta MetaVar
 | TForall [TypeVar] OIType
 deriving (Eq, Show)

data OIConstraint =
   CEq OIType OIType
 | CImp [MetaVar] [TypeVar] [OIConstraint] [OIConstraint]
