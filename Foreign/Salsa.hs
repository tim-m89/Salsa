-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Salsa: a .NET bridge for Haskell.
--
-----------------------------------------------------------------------------
module Foreign.Salsa (
    Obj, _Obj,
    null_, isNull,
    ( # ), (>>=#),
    new, invoke, set, get, delegate,
    AttrOp(..),
    withCLR, startCLR, stopCLR,
    Arr,         _Arr,
    Int32,
    cast, Coercible,
    Object_(..), _Object,
    Type_(..),   _Type,
    Int32_,      _Int32,
    String_,     _String
    ) where

import Foreign.Salsa.Core
import Foreign.Salsa.Common
import Foreign.Salsa.CLR

-- vim:set sw=4 ts=4 expandtab:
