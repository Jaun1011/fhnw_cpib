module Locations
  (BaseLocation(..), Loc(..), Location(Loc), Located,
   ErrorMsg, Checked,
   RC, rc2loc, spanRC, getLoc, getPhr, showPhrAtLoc)
where

import ErrorHandlingGeneric(ErrorMsgGen, CheckedGen)

import Data.Semigroup
import Data.Monoid

type Row = Int
type Col = Int

type RC   = (Row, Col)
type RCC  = (Row, (Col, Col))
type RCRC = (RC, RC)

data BaseLocation
  = Loc0 RC
  | Loc1 RCC
  | Loc2 RCRC
  deriving (Eq)

instance Show BaseLocation where
  show loc =
    case loc of
      Loc0 rc ->
        "(" ++ showRowCol rc ++ ")"
      Loc1 (r, (c1, c2)) ->
        "(" ++ show r ++ ":" ++ show c1 ++ "-" ++ show c2 ++ ")"
      Loc2 (r1c1, r2c2) ->
        "(" ++ showRowCol r1c1 ++ "-" ++ showRowCol r2c2 ++ ")"
    where showRowCol (r, c) = show r ++ ":" ++ show c

spanBaseLoc :: BaseLocation -> BaseLocation -> BaseLocation
locL `spanBaseLoc` locR = contract (expand locL `spanRCRC` expand locR)
  where
    expand :: BaseLocation -> RCRC
    expand (Loc0 rc)            = (rc, rc)
    expand (Loc1 (r, (c1, c2))) = ((r, c1), (r, c2))
    expand (Loc2 rcrc)          = rcrc

    spanRCRC :: RCRC -> RCRC -> RCRC
    (r1c1, _) `spanRCRC` (_, r2c2) = (r1c1, r2c2)

    contract :: RCRC -> BaseLocation
    contract rcrc@((r1, c1), (r2, c2)) =
      if r1 == r2 then
        if c1 == c2 then Loc0 (r1, c1)
        else Loc1 (r1, (c1, c2))
      else Loc2 rcrc

newtype Location = Loc (Maybe BaseLocation)
  deriving (Eq)

instance Show Location where
  show (Loc Nothing)        = "---"
  show (Loc (Just baseLoc)) = show baseLoc

instance Semigroup Location where
  Loc Nothing     <> Loc Nothing     = Loc Nothing
  Loc Nothing     <> Loc (Just loc2) = Loc (Just loc2)
  Loc (Just loc1) <> Loc Nothing     = Loc (Just loc1)
  Loc (Just loc1) <> Loc (Just loc2) = Loc (Just loc)
    where loc = loc1 `spanBaseLoc` loc2

instance Monoid Location where
  mempty = Loc Nothing

class (Eq l, Monoid l) => Loc l where
  noLoc :: l
  noLoc = Data.Monoid.mempty
  (<>) :: l -> l -> l
  (<>) = (Data.Semigroup.<>)

instance Loc Location

type Located a = (Location, a)
type ErrorMsg  = ErrorMsgGen Location
type Checked a = CheckedGen Location a

getLoc :: Located a -> Location
getLoc = fst
getPhr :: Located a -> a
getPhr = snd

rc2loc :: RC -> Location
rc2loc = Loc . Just . Loc0

spanRC :: RC -> RC -> Location
rc1 `spanRC` rc2 = (Loc . Just) (Loc0 rc1 `spanBaseLoc` Loc0 rc2)

showPhrAtLoc :: Show a => Located a -> [Char]
showPhrAtLoc (loc, phrase) = show phrase ++ " at " ++ show loc
