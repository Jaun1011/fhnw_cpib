-- IML, HS-2012, Ruedi
-- Edgar F.A. Lederer, FHNW
-- December 2020
-- March 2021

module Vm.Utilities where

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

fmapBoth f g = fmap (\(x, y) -> (f x, g y))
fmapFst  f   = fmapBoth f id
fmapSnd    g = fmapBoth id g

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

lookupSwap :: Eq b => b -> [(a, b)] -> Maybe a
lookupSwap y list = lookup y (map swap list)

takeWhilePlus :: (a -> Bool) -> [a] -> [a]
takeWhilePlus _ [] = []
takeWhilePlus p (x : xs)
  | p x = x : takeWhilePlus p xs
  | otherwise = [x]

flushRight :: Int -> String -> String
flushRight minWidth string =
  replicate (minWidth - length string) '\SP' ++ string

-- Theorem: minWidth <= length string ==> flushRight minWidth == id
-- Theorem: minWidth <= 0 ==> flushRight minWidth == id

testFlushRight =
  flushRight 3    "7" ==  "  7" &&
  flushRight 3   "70" ==  " 70" &&
  flushRight 3  "700" ==  "700" &&
  flushRight 3 "7000" == "7000" &&
  flushRight 0    "7" ==    "7" &&
  flushRight (-1) "7" ==    "7"
