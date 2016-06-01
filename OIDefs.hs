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
 deriving (Eq, Show)

data OIPat =
   PVar Name
 | PCon Name [OIPat]
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
   CEq OIType OIType
 | CImp [MetaVar] [TypeVar] [OIConstraint] [OIConstraint]
 deriving (Eq, Show)
