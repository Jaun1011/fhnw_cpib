-- BasicIML V03
-- HS-2012, Ruedi
-- Edgar F.A. Lederer, FHNW
-- March 2020
-- November 2020
-- January 2021

module Vm.ErrorHandlingGeneric where

newtype ErrorMsgGen l = ErrorMsg ([l], String)
  deriving Show

type CheckedGen l a = Either (ErrorMsgGen l) a
