{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
-- {-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, RankNTypes #-}

module MonadStatePlusClass  where

import Control.Monad.State
    
class (MonadState s m) => MonadStatePlus s m  where
    annotate :: String -> m s  -> m s 
    diagnostics :: m String

