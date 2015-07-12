{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
-- {-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MonadStatePlusWithClass(
                                StateMonadPlus
                              , runStateP
                              , showD
                              ,) where

import Control.Monad.State
import qualified Data.Map.Lazy as M
import Data.List
import Flow
--import qualified Debug.Trace as D
    
{- |
Some basic types
-}    

type Dic = M.Map String Integer
type Trace = Dic
-- data Trace = Trace Dic  deriving (Show)
-- unwrapT :: Trace -> Dic
-- unwrapT (Trace x) = x
-- liftT :: (Dic -> Dic) -> Trace -> Trace                   
-- liftT f t  = Trace $ f (unwrapT t)
showD :: Dic -> String
showD d =
    let r = concat ( d |> M.toList
                       |> map (\(kstr, vint)-> kstr ++ ":" ++ show vint ) 
                       |> intersperse ",")
    in "[" ++  r  ++ "]"

add :: Dic -> String -> Integer  ->  Dic            
add d entry n   =  M.insertWith (\_ old -> old+n) entry n d
inc ::  Dic -> String -> Dic            
inc d entry   =  add d entry 1 
-- incT :: String -> Trace -> Trace
-- incT = liftT .  inc

{- |
Constants
-}        
strDiag :: String                   
strDiag  = "diagnostic"

strReturn :: String
strReturn  = "return"

strBind :: String
strBind = "bind"
           
-- This wrapper is superflous. the point of it would be if we expose our functonality
-- in which case this would allow us to expose the *type* constructor (so that we can type things)
-- and hide the *value* constructor (so that only smart constructors would be allowed)
data StateMonadPlus s a  =  StateMonadPlus { unwrap :: StateT Trace (State s) a }

diagnostics :: StateMonadPlus s String
diagnostics =  StateMonadPlus $ do
                 trace  <- get
                 let newTrace =  inc trace strDiag
                 put newTrace
                 lift $ state (\s -> (showD newTrace, s))

modify' :: forall s a. (Dic -> Dic) -> StateMonadPlus s a -> StateMonadPlus s a
modify' f  m = StateMonadPlus $ StateT $  \trace ->
                   runStateT (unwrap m)  ( f trace) :: State s (a,Trace)
                   
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate msg = modify' (`inc` msg) 
                      
{- |
The equivalent for our outer monad of what 'state' is for State monad :"Embed a simple state action into the monad."
a function computes an a, might use and act on the trace, that's what a State monad is, so just build it 
we see that our external monad should be a writer not a state, as our computation should be independant of the log..
-}              
stateMP :: forall s a. (Trace -> s -> ((a, Trace),s)) -> StateMonadPlus s a
stateMP  f = StateMonadPlus $  StateT $ \trace -> 
               state $ f trace   :: State s (a,Trace)
                                    
-- pretty useless function....
-- except that it hides that we are using StateT inside
runStateP ::  forall s a.  StateMonadPlus s a -> s  -> ((a,Trace),s)
runStateP m   = runState $ runStateT (unwrap m) M.empty

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
-- automatically an instance of MonadState,                           
-- but we want to override that to expose the innner (not outer) state.
-- that makes it transparent for user to interpret a (StateMonadPlus s) in the State s monad, without log
-- or in the (StateMonadPlus s) monad, with log
instance MonadState s (StateMonadPlus s) where
     get = (StateMonadPlus $  StateT $ \trace -> do -- in (State s) now
                                         (a :: s ) <- (get :: State s s)
                                         return (a,trace) :: State s (s,Trace)) :: StateMonadPlus s s
     put s  = StateMonadPlus $  StateT $ \trace -> do
                                           put s
                                           return ((),trace)

exemple1 :: StateMonadPlus s String
exemple1 =
          do _ <- return (3::Int)  >> return (4::Int)
             _ <- return (5::Int)
             diagnostics
             
exemple2 :: StateMonadPlus s String
exemple2 = do _ <- annotate "A" (return  (3::Int) >>return (4::Int))
              _ <- return  (5::Int)
              a <- get
              diagnostics

exemple3 :: (MonadState s m) => m ()
exemple3 =
    do _ <- return (3::Int)  >> return (4::Int)
       _ <- return (5::Int)
       return ()
           
main :: IO ()
main =
    do
      let (a,_) = evalState  (runStateT (unwrap exemple1) M.empty)  (0::Integer)
      print a 
      let (_,s) = evalState  (runStateT (unwrap exemple3) M.empty)  (0::Integer)
      print(showD s) 
                  
      --putStrLn "hi"
