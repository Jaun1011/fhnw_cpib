-- BasicIML V03
-- Edgar F.A. Lederer, FHNW and Uni Basel, 2015
-- January 2020
-- December 2020
-- January 2021

module Vm.CheckedArithmetic
  (
    euclidDM, euclidQR, divE, modE, divF, modF, divT, modT,
    ArithError(DivisionByZero, Overflow),
    CheckedInt(negExcla, (+!), (-!), (*!),
      divEexcla, modEexcla, divFexcla, modFexcla, divTexcla, modTexcla),
    checkIntImplementation,
    Int32(..), Int64(..), Int1024(..),
    fromInt32toInt64, fromInt32toInt1024, fromInt64toInt1024,
    fromInt64toInt32, fromInt1024toInt32, fromInt1024toInt64, fromInt1024toInt,
    fromIntegerToInt32, fromIntegerToInt64, fromIntegerToInt1024,
  )
{-
  (
    negChecked, addChecked, subChecked, multChecked,
    divEuclidChecked, divFloorChecked, divTruncChecked,
    modEuclidChecked, modFloorChecked, modTruncChecked
  )
-}
where

euclidDM, euclidQR :: Integral a => a -> a -> (a, a)
-- Euclidean division defined via divMod
euclidDM a b
  | b > 0 || r == 0 = qr
  | otherwise       = (q+1, r-b)
  where qr@(q, r) = divMod a b

-- Euclidean division defined via quotRem
euclidQR a b
  | a >= 0 || r == 0 = qr
  | b > 0            = (q-1, r+b)
  | otherwise        = (q+1, r-b)
  where qr@(q, r) = quotRem a b

divE, modE :: Integral a => a -> a -> a
divE a b = fst (euclidDM a b)
modE a b = snd (euclidDM a b)

divF, modF, divT, modT :: Integral a => a -> a -> a
divF = div  -- floored
modF = mod
divT = quot -- truncated
modT = rem

data ArithError
  = DivisionByZero
  | Overflow
  deriving (Eq, Show)

class (Eq a, Ord a) => UncheckedInt a where
  negQuest :: a -> a
  (+?), (-?), (*?) :: a -> a -> a
  divEquest, modEquest :: a -> a -> a -- euclidean
  divFQuest, modFQuest :: a -> a -> a -- floored
  divTquest, modTquest :: a -> a -> a -- truncated
  fromIntegerQuest :: Integer -> a

class (Bounded a, UncheckedInt a) => CheckedInt a where
  negExcla :: a -> Either ArithError a
  (+!), (-!), (*!) :: a -> a -> Either ArithError a
  divEexcla, modEexcla :: a -> a -> Either ArithError a
  divFexcla, modFexcla :: a -> a -> Either ArithError a
  divTexcla, modTexcla :: a -> a -> Either ArithError a
  negExcla a
    | a == minBound = Left Overflow
    | otherwise     = Right (negQuest a)
  a +! b | isAddOk a b = Right (a +? b)
         | otherwise   = Left Overflow
    where isAddOk a b =
            b == fromIntegerQuest 0
              || (b > fromIntegerQuest 0 && a <= (maxBound -? b))
              || (b < fromIntegerQuest 0 && a >= (minBound -? b))
  a -! b | isSubOk a b = Right (a -? b)
         | otherwise   = Left Overflow
    where isSubOk a b =
            b == fromIntegerQuest 0
              || (b > fromIntegerQuest 0 && a >= (minBound +? b))
              || (b < fromIntegerQuest 0 && a <= (maxBound +? b))
  a *! b | isMultOk a b = Right (a *? b)
         | otherwise    = Left Overflow
    where isMultOk a b
            | a == fromIntegerQuest 0 = True
            | a > fromIntegerQuest 0 =
                if b >= fromIntegerQuest 0 then
                  b <= divTquest maxBound a
                else
                  b >= divTquest minBound a
            | otherwise =
                if b >= fromIntegerQuest 0 then
                  a == fromIntegerQuest (-1) || b <= divTquest minBound a
                else
                  b >= divTquest maxBound a
  divEexcla = divGenericExcla divEquest
  modEexcla = modGenericExcla modEquest
  divFexcla = divGenericExcla divFQuest
  modFexcla = modGenericExcla modFQuest
  divTexcla = divGenericExcla divTquest
  modTexcla = modGenericExcla modTquest

divGenericExcla ::
  CheckedInt a => (a -> a -> a) -> a -> a -> Either ArithError a
divGenericExcla divConcreteQuest a b
    | b == fromIntegerQuest 0 = Left DivisionByZero
    | a == minBound && b == fromIntegerQuest (-1) = Left Overflow
    | otherwise = Right (a `divConcreteQuest` b)
modGenericExcla ::
  CheckedInt a => (a -> a -> a) -> a -> a -> Either ArithError a
modGenericExcla modConcreteQuest a b
    | b == fromIntegerQuest 0 = Left DivisionByZero
    | a == minBound && b == fromIntegerQuest (-1) = Right (fromIntegerQuest 0)
    | otherwise = Right (a `modConcreteQuest` b)

checkIntImplementation :: String
checkIntImplementation =
  if (minBound :: Int) == -(2^31) && (maxBound :: Int) == (2^31)-1 then
    "Int has minBound and maxBound of 32 bit two's complement."
  else
  if (minBound :: Int) == -(2^63) && (maxBound :: Int) == (2^63)-1 then
    "Int has minBound and maxBound of 64 bit two's complement."
  else
    "Int has minBound and maxBound of unknown kind."

-- check whether Int suffices; otherwise, take Integer
newtype Int32 = Int32 { val32 :: Int }
  deriving (Eq, Ord, Show)
instance Bounded Int32 where
  minBound = Int32 (-(2^31))
  maxBound = Int32 ((2^31)-1)

-- check whether Int suffices; otherwise, take Integer
newtype Int64 = Int64 { val64 :: Int }
  deriving (Eq, Ord, Show)
instance Bounded Int64 where
  minBound = Int64 (-(2^63))
  maxBound = Int64 ((2^63)-1)

newtype Int1024 = Int1024 { val1024 :: Integer }
  deriving (Eq, Ord, Show)
instance Bounded Int1024 where
  minBound = Int1024 (-(2^1023))
  maxBound = Int1024 ((2^1023)-1)

instance UncheckedInt Int32 where
  negQuest (Int32 a) = Int32 (-a)
  Int32 a +? Int32 b = Int32 (a + b)
  Int32 a -? Int32 b = Int32 (a - b)
  Int32 a *? Int32 b = Int32 (a * b)
  Int32 a `divEquest` Int32 b = Int32 (a `divE` b)
  Int32 a `modEquest` Int32 b = Int32 (a `modE` b)
  Int32 a `divFQuest` Int32 b = Int32 (a `divF` b)
  Int32 a `modFQuest` Int32 b = Int32 (a `modF` b)
  Int32 a `divTquest` Int32 b = Int32 (a `divT` b)
  Int32 a `modTquest` Int32 b = Int32 (a `modT` b)
  fromIntegerQuest a = Int32 (fromInteger a)

instance UncheckedInt Int64 where
  negQuest (Int64 a) = Int64 (-a)
  Int64 a +? Int64 b = Int64 (a + b)
  Int64 a -? Int64 b = Int64 (a - b)
  Int64 a *? Int64 b = Int64 (a * b)
  Int64 a `divEquest` Int64 b = Int64 (a `divE` b)
  Int64 a `modEquest` Int64 b = Int64 (a `modE` b)
  Int64 a `divFQuest` Int64 b = Int64 (a `divF` b)
  Int64 a `modFQuest` Int64 b = Int64 (a `modF` b)
  Int64 a `divTquest` Int64 b = Int64 (a `divT` b)
  Int64 a `modTquest` Int64 b = Int64 (a `modT` b)
  fromIntegerQuest a = Int64 (fromInteger a)

instance UncheckedInt Int1024 where
  negQuest (Int1024 a) = Int1024 (-a)
  Int1024 a +? Int1024 b = Int1024 (a + b)
  Int1024 a -? Int1024 b = Int1024 (a - b)
  Int1024 a *? Int1024 b = Int1024 (a * b)
  Int1024 a `divEquest` Int1024 b = Int1024 (a `divE` b)
  Int1024 a `modEquest` Int1024 b = Int1024 (a `modE` b)
  Int1024 a `divFQuest` Int1024 b = Int1024 (a `divF` b)
  Int1024 a `modFQuest` Int1024 b = Int1024 (a `modF` b)
  Int1024 a `divTquest` Int1024 b = Int1024 (a `divT` b)
  Int1024 a `modTquest` Int1024 b = Int1024 (a `modT` b)
  fromIntegerQuest a = Int1024 (fromInteger a)

instance CheckedInt Int32
instance CheckedInt Int64
instance CheckedInt Int1024

fromInt32toInt64 :: Int32 -> Int64
fromInt32toInt64 (Int32 a) = Int64 a
fromInt32toInt1024 :: Int32 -> Int1024
fromInt32toInt1024 (Int32 a) = Int1024 (toInteger a)
fromInt64toInt1024 :: Int64 -> Int1024
fromInt64toInt1024 (Int64 a) = Int1024 (toInteger a)

fromInt64toInt32 :: Int64 -> Either ArithError Int32
fromInt64toInt32 (Int64 a)
  | minBound32 <= a && a <= maxBound32 = Right (Int32 a)
  | otherwise = Left Overflow
  where minBound32 = val32 minBound
        maxBound32 = val32 maxBound
fromInt1024toInt32 :: Int1024 -> Either ArithError Int32
fromInt1024toInt32 (Int1024 a)
  | minBound32 <= a && a <= maxBound32 =
      Right (Int32 (fromInteger a))
  | otherwise = Left Overflow
  where minBound32 = toInteger (val32 minBound)
        maxBound32 = toInteger (val32 maxBound)
fromInt1024toInt64 :: Int1024 -> Either ArithError Int64
fromInt1024toInt64 (Int1024 a)
  | minBound64 <= a && a <= maxBound64 =
      Right (Int64 (fromInteger a))
  | otherwise = Left Overflow
  where minBound64 = toInteger (val64 minBound)
        maxBound64 = toInteger (val64 maxBound)

fromInt1024toInt ::Int1024 -> Int
fromInt1024toInt (Int1024 a) = fromInteger a

fromIntegerToInt32 :: Integer -> Either ArithError Int32
fromIntegerToInt32 a
  | minBound32 <= a && a <= maxBound32 =
      Right (Int32 (fromInteger a))
  | otherwise = Left Overflow
  where minBound32 = toInteger (val32 minBound)
        maxBound32 = toInteger (val32 maxBound)

fromIntegerToInt64 :: Integer -> Either ArithError Int64
fromIntegerToInt64 a
  | minBound64 <= a && a <= maxBound64 =
      Right (Int64 (fromInteger a))
  | otherwise = Left Overflow
  where minBound64 = toInteger (val64 minBound)
        maxBound64 = toInteger (val64 maxBound)

fromIntegerToInt1024 :: Integer -> Either ArithError Int1024
fromIntegerToInt1024 a
  | minBound1024 <= a && a <= maxBound1024 =
      Right (Int1024 a)
  | otherwise = Left Overflow
  where minBound1024 = val1024 minBound
        maxBound1024 = val1024 maxBound
