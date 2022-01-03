module ErrorHandlingGeneric where

newtype ErrorMsgGen l = ErrorMsg ([l], String)
  deriving Show

type CheckedGen l a = Either (ErrorMsgGen l) a
