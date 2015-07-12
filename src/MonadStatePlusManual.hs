{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
module MonadStatePlusManual where

import Control.Monad.State

data Trace = Trace { bindCount :: Int , displayCount :: Int , returnCount :: Int }    
newtype StateMonadPlus s a =  StateMonadPlus { inner:: State (s, (Int, Int, Int)) a }

stateMP :: ((s, (Int, Int, Int)) -> (a, (s, (Int, Int, Int)))) -> StateMonadPlus s a
stateMP f = StateMonadPlus(state f)

runStateMP :: StateMonadPlus s a -> (s,(Int,Int,Int)) -> (a, (s, (Int, Int, Int)))
runStateMP smp = runState (inner smp)

-- one way to do it without MonadPlus            
instance Monad (StateMonadPlus s) where
    return e =  stateMP (\(s, (a,b,ret))->(e, (s, (a,b,ret+1))) )
    m >>= f  =  stateMP (\innerS ->  let (res, (s,(a,b,ret))) = runStateMP m innerS
                                     in  runStateMP (f res) (s,(a+1, b, ret)))

instance MonadState s (StateMonadPlus s) where -- 
    get = let r = stateMP (\(s,t) -> (s
                                     ,(s,t)))
          in r -- undefined  :: (StateMonadPlus s) s 
    put s = stateMP (\(_,t) -> ((),(s,t)))


            
diagnostics :: StateMonadPlus s String
diagnostics =  stateMP (\(s, (a,b,ret)) -> let newCounts = (a,b+1, ret) in (show newCounts, (s, newCounts)))
              
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate = undefined

                                                           

              
