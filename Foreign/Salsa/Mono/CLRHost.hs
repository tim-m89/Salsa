{-# LANGUAGE ForeignFunctionInterface, DoAndIfThenElse #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Mono specific boostraping and exports
--
-----------------------------------------------------------------------------
module Foreign.Salsa.Mono.CLRHost (
    startCLR',
    stopCLR',
    loadDriverAndBoot,
    SalsaString, withSalsaString, peekSalsaString
    ) where

import Data.Word
import Data.Int
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C
import System.IO
import System.Environment (getProgName)
import Control.Exception (bracket)
import qualified Data.ByteString.Unsafe as S
import Text.Printf

import Foreign.Salsa.Driver


data MonoAssembly = MonoAssembly
type MonoAssemblyPtr = Ptr MonoAssembly
data MonoDomain = MonoDomain
type MonoDomainPtr = Ptr MonoDomain
data MonoImage = MonoImage
type MonoImagePtr = Ptr MonoImage
data MonoMethodDesc = MonoMethodDesc
type MonoMethodDescPtr = Ptr MonoMethodDesc
data MonoClass = MonoClass
type MonoClassPtr = Ptr MonoClass
data MonoArray = MonoArray
type MonoArrayPtr = Ptr MonoArray
data MonoMethod = MonoMethod
type MonoMethodPtr = Ptr MonoMethod
data MonoObject = MonoObject
type MonoObjectPtr = Ptr MonoObject
data MonoAssemblyName = MonoAssemblyName
type MonoAssemblyNamePtr = Ptr MonoAssemblyName

type MonoImageOpenStatus = CInt

type GBool = CInt
gboolTrue = 1
gboolFalse = 0


foreign import ccall mono_jit_init :: CString -> IO MonoDomainPtr
foreign import ccall mono_jit_cleanup :: MonoDomainPtr -> IO ()
foreign import ccall mono_config_parse :: Ptr () -> IO ()
foreign import ccall mono_get_corlib :: IO MonoImagePtr
foreign import ccall mono_method_desc_new :: CString -> GBool -> IO MonoMethodDescPtr
foreign import ccall mono_method_desc_search_in_image :: MonoMethodDescPtr -> MonoImagePtr -> IO MonoMethodPtr
foreign import ccall mono_method_desc_free :: MonoMethodDescPtr -> IO ()
foreign import ccall mono_get_byte_class :: IO MonoClassPtr
foreign import ccall mono_array_new :: MonoDomainPtr -> MonoClassPtr -> Int -> IO MonoArrayPtr
foreign import ccall mono_value_copy_array :: MonoArrayPtr -> Int -> Ptr a -> Int -> IO ()
foreign import ccall mono_image_open_from_data :: Ptr a -> Word32 -> GBool -> Ptr MonoImageOpenStatus -> IO MonoImagePtr
foreign import ccall mono_runtime_invoke :: MonoMethodPtr -> Ptr a -> Ptr b -> Ptr c -> IO MonoObjectPtr
foreign import ccall mono_object_unbox :: MonoObjectPtr -> IO (Ptr a)
foreign import ccall mono_domain_get :: IO MonoDomainPtr
foreign import ccall mono_assembly_loaded :: MonoAssemblyNamePtr -> IO MonoAssemblyPtr
foreign import ccall mono_assembly_name_new :: CString -> IO MonoAssemblyNamePtr
foreign import ccall mono_assembly_get_image :: MonoAssemblyPtr -> IO MonoImagePtr
foreign import ccall mono_domain_set_config :: MonoDomainPtr -> CString -> CString -> IO ()


driverDataArray :: IO MonoArrayPtr
driverDataArray = unsafeUseAsCStringLen driverData $ \(p,l)-> do
    dom <- mono_domain_get
    if dom == nullPtr then
        error "null domain"
    else do 
        bcls <- mono_get_byte_class
        ar <- mono_array_new dom bcls l
        mono_value_copy_array ar 0 p l
        return ar


startCLR' = do
    mono_config_parse nullPtr
    domain <- return "salsa" >>= flip withCString mono_jit_init
    if (domain == nullPtr) then
        error "null domain"
    else do
        return ()

stopCLR' :: IO ()
stopCLR' = return ()


getSalsa :: IO MonoImagePtr
getSalsa = withCString "Salsa" $ \c-> do
    name <- mono_assembly_name_new c
    if name == nullPtr then
        error "Could not create assembly name"
    else do
        assem <- mono_assembly_loaded name
        if assem == nullPtr then
            error "Could not get Salsa assembly"
        else do
            image <- mono_assembly_get_image assem
            if image == nullPtr then
                error "Could not get Salsa image"
            else return image


getMethodFromNameImage :: String -> MonoImagePtr -> IO MonoMethodPtr
getMethodFromNameImage nameS img = withCString nameS $ \nameC-> do
    mthDes <- mono_method_desc_new nameC gboolTrue
    if mthDes == nullPtr then
        error "null mth des"
    else do
        method <- mono_method_desc_search_in_image mthDes img
        if method == nullPtr then
            error ("null method " ++ nameS)
        else return method

setupDomain :: IO ()
setupDomain = withCString "Salsa.config" $ \configFile-> do
    withCString "./" $ \baseDir-> do
        dom <- mono_domain_get
        mono_domain_set_config dom baseDir configFile
        return ()

loadDriverAndBoot :: IO (FunPtr (CString -> IO (FunPtr a)))
loadDriverAndBoot = do
    setupDomain
    loadDriver
    salsa <- getSalsa
    method <- getMethodFromNameImage "Salsa.Driver:Boot()" salsa
    if method == nullPtr then
        error "Could not find boot method"
    else do
        oret <- mono_runtime_invoke method nullPtr nullPtr nullPtr
        pret <- mono_object_unbox oret
        ret <- peek pret
        return ret

loadDriver :: IO ()
loadDriver = do
    corlib <- mono_get_corlib
    if (corlib == nullPtr) then
        error "null image"
    else do
        method <- getMethodFromNameImage "System.Reflection.Assembly:Load(byte[])" corlib
        if (method == nullPtr) then
            error "Cannot find method"
        else do
            dd <- driverDataArray
            withArray [dd] $ \argp-> do
                mono_runtime_invoke method nullPtr argp nullPtr
                return ()



type SalsaString = CString
withSalsaString = withCString
peekSalsaString = peekCString






-- vim:set ts=4 sw=4 expandtab:
