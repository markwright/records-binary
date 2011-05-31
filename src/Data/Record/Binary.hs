{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeOperators #-}

module Data.Record.Binary where

import Data.Kind
import Data.TypeFun
import Data.Record
import Data.Record.Combinators
import Data.Binary

-- | Empty record scheme.  X only has one constructor which has no parameters, hence serialization of X is unneccessary.
instance Binary style => 
  Binary (X style) where
    -- | put X does not need to write anything.
    put X = return ()
    -- | get X just returns X without needing to read anything.
    get = return X

-- | Serialize non-empty record schemes.
instance (Binary (rec style), Binary name, Binary (App style sort)) => 
  Binary ((rec :& name ::: sort) style) where
    -- | put non-empty record scheme
    put (rec :& field) = put rec >> put field >> return ()
    -- | get non-empty record scheme
    get = get >>= (\rec -> get >>= (\field -> return (rec :& field)))

-- | Serialize record fields.
instance (Binary name, Binary (App style sort)) =>
  Binary ((name ::: sort) style) where
    -- | put record field
    put (name := val) = put name >> put val >> return ()
    -- | get record field
    get = get >>= (\name -> get >>= (\val -> return (name := val)))
