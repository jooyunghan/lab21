{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
-- {-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MonadStatePlus(
                                StateMonadPlus
                              , runStateP
                              , showD
                              ) where
import MonadStatePlusClass
import Types
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map.Lazy as M
import Data.List

-- je fais StateMonadPlus instance de MonadStatePlus
-- je generalise StateMonadPlus vers StatePlusT m et StateMonadPlus = StatePlusT (Identity ou State?)

    
data StateMonadPlus s a  =  StateMonadPlus { unwrap :: StateT Trace (StateT s Identity) a }

instance Monad (StateMonadPlus s) where
    return :: forall a. a -> StateMonadPlus s a
    return e = StateMonadPlus $  StateT $ \trace -> do
                let trace' = inc trace strReturn 
                state (\s -> ((e,trace'),s) )  :: State s (a,Trace)
                                                  
    (>>=) :: forall a b. StateMonadPlus s a -> (a -> StateMonadPlus s b) -> StateMonadPlus s b
    m >>= f  = StateMonadPlus $  StateT $ \trace -> do -- we run in the (State s) monad now
                                     let trace' = inc trace strBind
                                     (a,trace'') <- runStateT (unwrap m) trace' :: State s (a,Trace)
                                     let step  = f a :: StateMonadPlus s b
                                     (b, trace''') <-  runStateT (unwrap step) trace'' :: State s (b,Trace)
                                     return (b,  trace''' ) :: State s (b,Trace)

    fail ::  String -> (StateMonadPlus s) a
    fail msg = undefined

                         
instance MonadState s (StateMonadPlus s) where
     get = (StateMonadPlus $  StateT $ \trace -> do -- in (State s) now
                                         (a :: s ) <- (get :: State s s)
                                         return (a,trace) :: State s (s,Trace)) :: StateMonadPlus s s
     put s  = StateMonadPlus $  StateT $ \trace -> do
                                           put s
                                           return ((),trace)
 
instance MonadStatePlus s (StateMonadPlus s) where                                                  
    diagnostics :: StateMonadPlus s String
    diagnostics =  StateMonadPlus $ do
                 trace  <- get
                 let newTrace =  inc trace strDiag
                 put newTrace
                 lift $ state (\s -> (showD newTrace, s))                   
    annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
    annotate msg = modify' (`inc` msg) 
               where
                 modify' :: forall s a. (Dic -> Dic) -> StateMonadPlus s a -> StateMonadPlus s a
                 modify' f  m = StateMonadPlus $ StateT $  \trace ->
                       runStateT (unwrap m)  ( f trace) :: State s (a,Trace)


stateMP :: forall s a. (Trace -> s -> ((a, Trace),s)) -> StateMonadPlus s a
stateMP  f = StateMonadPlus $  StateT $ \trace -> state $ f trace   :: State s (a,Trace)
                                    
runStateP ::  forall s a.  StateMonadPlus s a -> s  -> ((a,Trace),s)
runStateP m   = runState $ runStateT (unwrap m) M.empty



