{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses  #-}
-- {-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, RankNTypes #-}

module MonadStatePlusClass  where

import Control.Monad.State
    
class (MonadState s m) => MonadStatePlus s m | m -> s where
    annotate :: String -> m a  -> m a 
    diagnostics :: m String

