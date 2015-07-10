{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
module Main where

import Control.Monad.State 

newtype StateMonadPlus s a =  StateMonadPlus { inner:: State (s, (Int, Int, Int)) a }

stateMP :: ((s, (Int, Int, Int)) -> (a, (s, (Int, Int, Int)))) -> StateMonadPlus s a
stateMP f = StateMonadPlus(state f)

runStateMP :: StateMonadPlus s a -> (s,(Int,Int,Int)) -> (a, (s, (Int, Int, Int)))
runStateMP smp = runState (inner smp)

instance Monad (StateMonadPlus s) where
    return e =  stateMP (\(s, (a,b,ret))->(e, (s, (a,b,ret+1))) )
    m >>= f  =  stateMP (\innerS ->  let (res, (s,(a,b,ret))) = runStateMP m innerS
                                   in  runStateMP (f res) (s,(a+1, b, ret)))

instance MonadState s (StateMonadPlus s) where
    get = undefined
    put = undefined

          
diagnostics :: StateMonadPlus s String
diagnostics =  stateMP (\(s, (a,b,ret)) -> let newCounts = (a,b+1, ret) in (show newCounts, (s, newCounts))) 

              
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate = undefined

exemple1 :: StateMonadPlus s String
exemple1 = do return 3  >> return 4
              return 5
              diagnostics
           
main :: IO ()
main =
    do
      let (a,s) = runStateMP  exemple1 ("",(0,0,0))
      putStrLn (show a)
