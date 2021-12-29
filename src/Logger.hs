module Logger (
    info,
    logIdent,
) where


import Debug.Trace ( trace )

info :: (Show a, Monad m) => String ->  a -> m a
info = logIdent 0


logIdent :: (Show a, Monad m) => Int -> String ->  a -> m a
logIdent i msg x = trace ((replicate i '\t') ++ "["++msg ++ "]\t"++ show x) (return x)
