{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
module Main where

import Control.Applicative
import Control.Monad.State 
import Control.Monad 

data Diag = Diag {
  label:: Maybe String,
  n_returns:: Int,
  n_binds:: Int,
  n_diagnostics:: Int
} deriving (Eq, Show)

emptyDiag = Diag Nothing 0 0 0

addDiagnostics d@Diag{n_diagnostics=n} = d{n_diagnostics=n+1}

addReturn d@Diag{n_returns=n} = d{n_returns=n+1}

addBind d@Diag{n_binds=n} = d{n_binds=n+1}

setLabel :: String -> Diag -> Diag
setLabel s d = d{label=Just s}

data SMP m a = SMP { runSMP:: StateT Diag m a }

instance (Monad m) => Functor (SMP m) where
  fmap = liftM 

instance (Monad m) => Applicative (SMP m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (SMP m) where
  return x = SMP $ do
    modify addReturn
    return x
  m >>= f = SMP $ do
    modify addBind
    a <- runSMP m
    runSMP (f a)

type StateMonadPlus s a = SMP (State s) a

diagnostics :: StateMonadPlus s String
diagnostics = SMP $ StateT $ \diag -> 
  let diag' = addDiagnostics diag
  in state(\s -> ((show diag', diag'),s)) 

annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate s smp = SMP $ do
  modify (setLabel s)
  runSMP smp

example1 :: StateMonadPlus s String
example1 = do return 3  >> return 4
              return 5
              diagnostics

example2 :: StateMonadPlus s String
example2 = do annotate "A" (return 3 >> return 4)
              return 5
              diagnostics

main :: IO ()
main = do
      let diag1 = evalState (execStateT (runSMP example1) emptyDiag) 0
      putStrLn ("example1 " ++ show diag1)
      let diag2 = evalState (execStateT (runSMP example2) emptyDiag) 0
      putStrLn ("example2 " ++ show diag2)