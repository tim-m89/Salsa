{-# LANGUAGE EmptyDataDecls #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Exports data types and functions that are used throughout the bridge
-- implementation.
--
-----------------------------------------------------------------------------
module Foreign.Salsa.Common (
    Obj(..),          _Obj,
    Null,
    Object_(..),      _Object,
    Type_(..),        _Type,
    Int32,   Int32_,  _Int32,
    String,  String_, _String,
    Arr,              _Arr,
    Array_,           _Array
    ) where

import Foreign hiding (new, newForeignPtr)

import Foreign.Salsa.TypePrelude
import Foreign.Salsa.CLR

-- | @Obj a@ represents a .NET object instance of type @a@.
data Obj a = Obj !ObjectId !(ForeignPtr ()) 
           | ObjNull

data Null

-- Value-level equivalent of Obj type constructor:
_Obj :: t -> Obj t
_Obj _ = undefined

instance Show (Obj a) where
    show (Obj id _) = "Object(" ++ show id ++ ")"
    show ObjNull    = "Object(null)"

-- Labels for elementary .NET types
data Object_ = Object_                  -- System.Object
data Type_   = Type_                    -- System.Type
data Array_  = Array_                   -- System.Array
_Type   = undefined :: Type_
_Object = undefined :: Object_
_Array  = undefined :: Array_

-- Synonym labels for the primitive types
type String_ = String
type Int32_  = Int32
_String = undefined :: String_
_Int32  = undefined :: Int32_

-- | Represents .NET array types of element type @t@.
data Arr t

-- -- Value-level equivalent of Arr type constructor:
_Arr :: t -> Arr t
_Arr t = undefined

-- vim:set sw=4 ts=4 expandtab:
