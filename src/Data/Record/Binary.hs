-- Copyright (c) 2011, Mark Wright.  All rights reserved.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeOperators #-}

-- | Binary instances for Data.Record
module Data.Record.Binary where

import Data.Kind
import Data.TypeFun
import Data.Record
import Data.Record.Combinators
import Data.Binary

-- | Serialization of KindStar is unneccessary.
instance Binary KindStar where
  -- | put KindStar does not need to write anything.
  put KindStar = return ()
  -- | get KindStar just returns KindStar without needing to read anything.
  get = return KindStar

-- | Serialization of Id KindStar is unneccessary.
instance Binary KindStar =>
  Binary (Id KindStar) where
    -- | put Id KindStar does not need to write anything.
    put (Id KindStar) = return ()
    -- | get Id KindStar just returns Id KindStar without needing to read anything.
    get = return (Id KindStar)

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
