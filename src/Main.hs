{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
module Main where

import Control.Monad.State
--import qualified Data.Map.Lazy as M
import qualified MonadStatePlus as V2
import qualified MonadStatePlusManual as V1


exemple1 :: V1.StateMonadPlus s String
exemple1 = do _<- return (3::Int)  >> return (4::Int)
              _<- return (5::Int)
              V1.diagnostics

exemple2 :: V1.StateMonadPlus s String
exemple2 = do _ <- V1.annotate "A" (return  (3::Int) >>return (4::Int))
              _ <- return  (5::Int)
              a <- get
              V1.diagnostics
              
exemple3 :: (MonadState s m) => m String
exemple3 =
    do _ <- return (3::Int)  >> return (4::Int)
       _ <- return (5::Int)
       _ <- get
       return "hi"
              
exemple4 :: (MonadState s m) => m String
exemple4 =
    do _ <- return (3::Int)  >> return (4::Int)
       _ <- return (5::Int)
       _ <- get
       _<- fail "I am a total failure"
       return "hi"
              
main :: IO ()
main =
    do
      let (_,t) = V1.runStateMP  exemple3 ("",(0,0,0))
      putStrLn ("first version : " ++ show t )
      let (a,is) =  runState  exemple3   (0::Integer)
      putStrLn ("second version in state monad : result " ++ show a ++ ", state :" ++ show is )
      let ((a,t),is) =  V2.runStateP  exemple3   (0::Integer)
      putStrLn ("second version in stateP monad: result " ++ show a ++ ", state :" ++ show is ++ ", log :" ++ V2.showD  t )
