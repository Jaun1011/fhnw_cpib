-- BasicIML V03
-- Edgar F.A. Lederer, FHNW
-- January 2020
-- December 2020
-- January 2021
-- March 2021

module BaseDecls where

import Locations(Loc(..), Located)
import CheckedArithmetic(Int32(..), Int64(..), Int1024(..))

import Data.Maybe(fromMaybe)

type LocIdent = Located Ident
newtype Ident = Ident String
  deriving Eq

instance Show Ident where
  show (Ident ident) = "'" ++ ident ++ "'"

type LocTypedIdent = Located TypedIdent
newtype TypedIdent = TypedIdent (LocIdent, LocType)
  deriving (Eq, Show)

type LocValue a = Located (Value a)
data Value a
  = BoolVal Bool
  | IntVal a
  deriving Eq

instance Show a => Show (Value a) where
  show (BoolVal b) = show b
  show (IntVal i)  = show i

-- The introduction of newtype Integer' seems superfluous.
-- However, it ensures by type checking that none of the conversions in the
-- intLiteralChecker is accidentally forgotten.

newtype Integer' = Integer' Integer
  deriving (Eq, Show)

data IntValue
  = Int32Val Int32
  | Int64Val Int64
  | Int1024Val Int1024
  deriving Eq

instance Show IntValue where
  show (Int32Val i)   = show (val32 i)
  show (Int64Val i)   = show (val64 i)
  show (Int1024Val i) = show (val1024 i)

type LocType = Located Type
data Type
  = BoolTy
  | IntTy WordLength
  deriving Eq

data WordLength
  = WL32
  | WL64
  | WL1024
  deriving (Eq, Ord, Show)

instance Show Type where
  show BoolTy         = "bool"
  show (IntTy WL32)   = "int32"
  show (IntTy WL64)   = "int64"
  show (IntTy WL1024) = "int1024"

type LocOperator = Located Operator
data Operator
  = Not
  | Times
  | DivE -- euclidean
  | ModE
  | DivF -- floored
  | ModF
  | DivT -- truncated
  | ModT
  | Plus
  | Minus
  | Less
  | GreaterEq
  | Equal
  | NotEq
  | Greater
  | LessEq
  | CondAnd
  | CondOr
  deriving Eq

instance Show Operator where
  show Not       = "~"
  show Times     = "*"
  show DivE      = "divE"
  show ModE      = "modE"
  show DivF      = "divF"
  show ModF      = "modF"
  show DivT      = "divT"
  show ModT      = "modT"
  show Plus      = "+"
  show Minus     = "-"
  show Less      = "<"
  show GreaterEq = ">="
  show Equal     = "="
  show NotEq     = "/="
  show Greater   = ">"
  show LessEq    = "<="
  show CondAnd   = "/\\?"
  show CondOr    = "\\/?"

isDivOpr :: Operator -> Bool
isDivOpr DivE = True
isDivOpr ModE = True
isDivOpr DivF = True
isDivOpr ModF = True
isDivOpr DivT = True
isDivOpr ModT = True
isDivOpr _    = False

isArithOpr :: Operator -> Bool
isArithOpr Plus  = True
isArithOpr Minus = True
isArithOpr Times = True
isArithOpr opr   = isDivOpr opr

isEqualityOpr :: Operator -> Bool
isEqualityOpr Equal = True
isEqualityOpr NotEq = True
isEqualityOpr _     = False

isOrderingOpr :: Operator -> Bool
isOrderingOpr Less      = True
isOrderingOpr GreaterEq = True
isOrderingOpr Greater   = True
isOrderingOpr LessEq    = True
isOrderingOpr _         = False

isRelOpr :: Operator -> Bool
isRelOpr opr = isEqualityOpr opr || isOrderingOpr opr

isBoolOpr :: Operator -> Bool
isBoolOpr CondAnd = True
isBoolOpr CondOr  = True
isBoolOpr _       = False

type LocInitMode = Located InitMode
data InitMode
  = DoInit
  | DontInit
  deriving (Eq, Show)

type LocFlowMode = Located FlowMode
data FlowMode
  = InFlow
  | InoutFlow
  | OutFlow
  deriving (Eq, Show)

defaultFlowMode :: Maybe LocFlowMode -> LocFlowMode
defaultFlowMode optLocFlowMode = fromMaybe (noLoc, InFlow) optLocFlowMode

type LocMechMode = Located MechMode
data MechMode
  = CopyMech
  | RefMech
  deriving (Eq, Show)

defaultMechMode :: Maybe LocMechMode -> LocMechMode
defaultMechMode optLocMechMode = fromMaybe (noLoc, CopyMech) optLocMechMode

type LocChangeMode = Located ChangeMode
data ChangeMode
  = Const
  | Var
  deriving (Eq, Show)

defaultChangeMode :: Maybe LocChangeMode -> LocChangeMode
defaultChangeMode optLocChangeMode = fromMaybe (noLoc, Const) optLocChangeMode

isOutFlow, isReadFlow, isInFlow, isWriteFlow :: FlowMode -> Bool
isOutFlow OutFlow = True
isOutFlow _       = False

isReadFlow = not . isOutFlow -- readFlow is thus InFlow or InoutFlow

isInFlow InFlow = True
isInFlow _      = False

isWriteFlow = not . isInFlow -- writeFlow is thus InoutFlow or OutFlow

isOutgoingCopy :: FlowMode -> MechMode -> Bool
isOutgoingCopy InoutFlow CopyMech = True
isOutgoingCopy OutFlow   CopyMech = True
isOutgoingCopy _         _        = False

data Scope
  = Global
  | Local
  deriving (Eq, Show)

data AccessMech
  = Direct
  | Indirect
  deriving (Eq, Show)

data AccessMode
  = Read
  | Update
  | Init
  deriving (Eq, Show)

data RoutineSort
  = Fun
  | Proc
  deriving Eq

instance Show RoutineSort where
  show Fun  = "function"
  show Proc = "procedure"
