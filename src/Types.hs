module Types where

import Flow
import qualified Data.Map.Lazy as M
import Data.List
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
