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
 deriving (Eq, Show)

data OIConstraint =
   CEq OIType OIType                                       -- t1 ~ t2
 | CImp [MetaVar] [TypeVar] [OIConstraint] [OIConstraint]  -- [alphas] \forall betas Cs ⊃ Fs
 deriving (Eq, Show)

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
