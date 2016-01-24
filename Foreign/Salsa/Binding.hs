{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
--
-- Exports data types and functions required by the generated binding files.
--
-----------------------------------------------------------------------------
module Foreign.Salsa.Binding (
    module Foreign.Salsa.Common,
    module Foreign.Salsa.Core,
    module Foreign.Salsa.CLR,
    module Foreign.Salsa.Resolver,
    FunPtr, unsafePerformIO, liftM,
    type_GetType
    ) where

import Foreign.Salsa.Common
import Foreign.Salsa.Core
import Foreign.Salsa.CLR
import Foreign.Salsa.Resolver

import System.IO.Unsafe ( unsafePerformIO )
import Foreign hiding (new, unsafePerformIO)
import Foreign.C.String

import Control.Monad (liftM)

-- TODO: Perhaps move some/all of this into the generator, so that it can be
--       CLR-version neutral.

--
-- Import the System.Type.GetType(String) and System.Type.MakeArrayType(Int32)
-- methods so they can be used in the implementations of 'typeOf' as produced by
-- the generator.
--

type Type_GetType_stub = SalsaString -> Bool -> IO ObjectId
foreign import ccall "dynamic" make_Type_GetType_stub :: FunPtr Type_GetType_stub -> Type_GetType_stub

{-# NOINLINE type_GetType_stub #-}
type_GetType_stub :: Type_GetType_stub
type_GetType_stub = make_Type_GetType_stub $ unsafePerformIO $ getMethodStub
    "System.Type, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "GetType"
    "System.String;System.Boolean"

type_GetType typeName = marshalMethod2s type_GetType_stub undefined undefined (typeName, True)


type Type_MakeArrayType_stub = ObjectId -> Int32 -> IO ObjectId
foreign import ccall "dynamic" make_Type_MakeArrayType_stub :: FunPtr Type_MakeArrayType_stub -> Type_MakeArrayType_stub

{-# NOINLINE type_MakeArrayType_stub #-}
type_MakeArrayType_stub :: Type_MakeArrayType_stub
type_MakeArrayType_stub = make_Type_MakeArrayType_stub $ unsafePerformIO $ getMethodStub
    "System.Type, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "MakeArrayType"
    "System.Int32"

-- 
-- SalsaForeignType instances for primitive types
--

instance SalsaForeignType Int32  where foreignTypeOf _ = foreignTypeFromString "System.Int32"
instance SalsaForeignType String where foreignTypeOf _ = foreignTypeFromString "System.String"
instance SalsaForeignType Bool   where foreignTypeOf _ = foreignTypeFromString "System.Boolean"
instance SalsaForeignType Double where foreignTypeOf _ = foreignTypeFromString "System.Double"

foreignTypeFromString :: String -> Obj Type_
foreignTypeFromString s = unsafePerformIO $ do
--    putStrLn $ "typeOfString: " ++ s
    type_GetType s
--    marshalMethod1s type_GetType_stub undefined undefined s

-- Define the foreignTypeOf function for arrays by first calling foreignTypeOf on the element type,
-- and then using the Type.MakeArrayType method to return the associated one-dimensional
-- array type:
instance SalsaForeignType t => SalsaForeignType (Arr t) where
  foreignTypeOf _ = unsafePerformIO $
    marshalMethod1i type_MakeArrayType_stub (foreignTypeOf (undefined :: t)) undefined (1 :: Int32)
    -- Note: this function requires ScopedTypeVariables

-- Define foreignTypeOf for reference types in terms of the associated static type.
instance SalsaForeignType t => SalsaForeignType (Obj t) where
  foreignTypeOf _ = foreignTypeOf (undefined :: t)

-- vim:set sw=4 ts=4 expandtab:
