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
import Control.Applicative
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Error
import qualified Data.Map.Lazy as M
--import Data.List
-- je fais StateMonadPlus instance de MonadStatePlus
-- je generalise StateMonadPlus vers StatePlusT m et StateMonadPlus = StatePlusT (Identity ou State?)

-- transformer assumes effect are orthogonals. but we might want to stop logging when a computation fails.
-- if we want that, we need to put the dependencies higher, as they ll be available first to depend upon

-- Either String ((a,Trace),s)   
--data StateMonadPlus s a  =  StateMonadPlus { unwrap :: StateT Trace (StateT s (ErrorT String Identity)) a }
-- ((Either a, Trace),s)   
data StateMonadPlus s a  =  StateMonadPlus { unwrap :: ErrorT String (StateT Trace  (StateT s Identity)) a }
--- data DiagT  m a  =  DiagT { unwrap :: ErrorT String (StateT Trace  m ) a }
--- instance (Monad m) => Monad (DiagT m) where return ....
--- type StateMonadPlus s = DiagT (State s)

instance Monad (StateMonadPlus s) where
    return :: forall a. a -> StateMonadPlus s a
    return e = StateMonadPlus $  ErrorT  $ StateT $ \trace ->  StateT $ \s -> do
               let trace' = inc trace strReturn
               return ((Right e,trace'),s)
                                                  
    (>>=) :: forall a b. StateMonadPlus s a -> (a -> StateMonadPlus s b) -> StateMonadPlus s b
    m >>= f  = StateMonadPlus $  ErrorT $ StateT $ \trace -> do -- we run in the (State s) monad now
                 let trace' = inc trace strBind
                 (a,trace'') <- runStateT (runErrorT (unwrap m)) trace'
                 case a of
                    Left msg -> return (Left msg, trace'')
                    Right a -> do
                        let step  = f a :: StateMonadPlus s b
                        runStateT (runErrorT (unwrap step)) trace''
                              

    fail ::  String -> (StateMonadPlus s) a
    fail msg = StateMonadPlus $ throwError msg

                         
instance MonadState s (StateMonadPlus s) where
     get = StateMonadPlus $  ErrorT $ StateT $ \trace -> do -- in (State s) now
                                         a  <- get
                                         return  (Right a,trace)
     put s  = StateMonadPlus $ ErrorT $  StateT $ \trace -> do 
                                           put s
                                           return (Right (),trace)
 
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
                 modify' f  m = StateMonadPlus  $  ErrorT $ StateT $  \trace -> 
                                 runStateT (runErrorT (unwrap m)) (f trace)

                                    
runStateP ::  forall s a.  StateMonadPlus s a -> s  ->  ((Either String a,Trace),s)
runStateP m  s = runIdentity $ runStateT (runStateT (runErrorT (unwrap m)) M.empty) s


