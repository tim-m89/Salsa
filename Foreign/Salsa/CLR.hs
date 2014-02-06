{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Provides convenient functions for accessing the CLR, including: loading
-- the CLR into the process, releasing .NET object references, and obtaining
-- dynamically-generated stub functions for calling into .NET from Haskell.
--
-----------------------------------------------------------------------------
module Foreign.Salsa.CLR (
    withCLR,
    startCLR, stopCLR,
    ObjectId,
    releaseObject,
    getMethodStub,
    getFieldGetStub,
    getFieldSetStub,
    getDelegateConstructorStub,
    boxString, boxInt32, boxBoolean,
    SalsaString, withSalsaString, peekSalsaString 
    ) where

import Data.Int
import System.IO.Unsafe ( unsafePerformIO )
import Foreign hiding ( new, newForeignPtr, unsafePerformIO )
import Foreign.C.String

#if (MONO)
import Foreign.Salsa.Mono.CLRHost
#else
import Foreign.Salsa.Win.CLRHost
#endif

-- | Identifies a foreign (.NET) object instance
type ObjectId = Int32

-- | Starts the .NET execution engine before executing the given IO action, and
--   finally stopping the execution engine.  This can only be performed once
--   in a process.
withCLR :: IO a -> IO a
withCLR action = do
    startCLR
    r <- action
    stopCLR
    return r

startCLR :: IO ()
startCLR = do
    startCLR'
    -- Allow .NET to call into Haskell and free unused function pointer wrappers
    setFreeHaskellFunPtr 

stopCLR :: IO ()
stopCLR = do
    -- saveDynamicAssembly -- (for debugging)

    -- Prevent .NET finalizers from calling into Haskell (and causing access violations)
    clearFreeHaskellFunPtr

    stopCLR'



-- | @'unsafeGetPointerToMethod' m@ returns a function pointer to the method @m@ 
--  as implemented in the Salsa .NET driver assembly (Salsa.dll).  It is safe only
--  if the type of the resulting function pointer matches that of the method given.
unsafeGetPointerToMethod :: String -> IO (FunPtr a)
unsafeGetPointerToMethod methodName = do
    result <- withSalsaString methodName $ \methodName' -> getPointerToMethodRaw methodName'
    if result == nullFunPtr
        then error $ "Unable to execute Salsa.dll method '" ++ methodName ++ "'."
        else return result

{-# NOINLINE getPointerToMethodRaw #-}
getPointerToMethodRaw :: GetPointerToMethodDelegate a
getPointerToMethodRaw = makeGetPointerToMethodDelegate $ unsafePerformIO $ loadDriverAndBoot

type GetPointerToMethodDelegate a = SalsaString -> IO (FunPtr a)
foreign import ccall "dynamic" makeGetPointerToMethodDelegate :: FunPtr (GetPointerToMethodDelegate a) ->
    GetPointerToMethodDelegate a


-- | Releases the .NET object indicated by the given object id.
{-# NOINLINE releaseObject #-}
releaseObject :: ObjectId -> IO ()
releaseObject = makeReleaseObjectDelegate $ unsafePerformIO $ unsafeGetPointerToMethod "ReleaseObject"

type ReleaseObjectDelegate = ObjectId -> IO ()
foreign import ccall "dynamic" makeReleaseObjectDelegate :: FunPtr ReleaseObjectDelegate -> ReleaseObjectDelegate


-- | Passes a function pointer to the 'freeHaskellFunPtr' function into .NET so
--   that Haskell FunPtr's can be freed from .NET code.
setFreeHaskellFunPtr :: IO ()
setFreeHaskellFunPtr = do
    funPtr <- wrapFreeHaskellFunPtr freeHaskellFunPtr
    setFreeHaskellFunPtrRaw funPtr
    -- Note: since the function passed into .NET may be used by .NET at any
    --       point until the engine is shutdown, and the engine is only loaded
    --       once per process, we don't need to free it.


-- | Clears the 'freeHaskellFunPtr' pointer on the .NET side to prevent finalizers from
--   calling into Haskell (and causing access violations).
clearFreeHaskellFunPtr :: IO ()
clearFreeHaskellFunPtr = setFreeHaskellFunPtrRaw nullFunPtr


{-# NOINLINE setFreeHaskellFunPtrRaw #-}
setFreeHaskellFunPtrRaw :: (FunPtr (FunPtr a -> IO ()) -> IO ())
setFreeHaskellFunPtrRaw = makeSetFreeHaskellFunPtrDelegate $ unsafePerformIO $
    unsafeGetPointerToMethod "SetFreeHaskellFunPtr"
  
foreign import ccall "dynamic" makeSetFreeHaskellFunPtrDelegate ::
    FunPtr (FunPtr (FunPtr a -> IO ()) -> IO ()) -> (FunPtr (FunPtr a -> IO ()) -> IO ())

foreign import ccall "wrapper" wrapFreeHaskellFunPtr :: 
    (FunPtr a -> IO ()) -> IO (FunPtr (FunPtr a -> IO ()))


-- | 'saveDynamicAssembly' saves the assembly containing the dynamically-generated
--   wrapper stubs to disk (for debugging purposes).
{-# NOINLINE saveDynamicAssembly #-}
saveDynamicAssembly :: IO ()
saveDynamicAssembly = makeSaveDynamicAssemblyDelegate $ unsafePerformIO $ unsafeGetPointerToMethod "SaveDynamicAssembly"

type SaveDynamicAssemblyDelegate = IO ()
foreign import ccall "dynamic" makeSaveDynamicAssemblyDelegate :: FunPtr SaveDynamicAssemblyDelegate -> SaveDynamicAssemblyDelegate


-- | @'getMethodStub' c m s@ returns a function pointer to a function that, when
--   called, invokes the method with name @m@ and signature @s@ in class @c@.
--
--   @s@ should be a semi-colon delimited list of parameter types indicating the
--   desired overload of the given method.
getMethodStub :: String -> String -> String -> IO (FunPtr f)
getMethodStub className methodName parameterTypeNames = do
    withSalsaString className $ \className' ->
        withSalsaString methodName $ \methodName' ->
            withSalsaString parameterTypeNames $ \parameterTypeNames' ->
                return $ getMethodStubRaw className' methodName' parameterTypeNames'

{-# NOINLINE getMethodStubRaw #-}
getMethodStubRaw :: GetMethodStubDelegate a
getMethodStubRaw = makeGetMethodStubDelegate $ unsafePerformIO $ unsafeGetPointerToMethod "GetMethodStub"

type GetMethodStubDelegate a = SalsaString -> SalsaString -> SalsaString -> FunPtr a
foreign import ccall "dynamic" makeGetMethodStubDelegate :: FunPtr (GetMethodStubDelegate a) ->
    (GetMethodStubDelegate a)


-- | @'getFieldGetStub' c f@ returns a function pointer to a function that, when
--   called, gets the value of the field @f@ in class @c@.
getFieldGetStub :: String -> String -> IO (FunPtr f)
getFieldGetStub className fieldName = do
    withSalsaString className $ \className' ->
        withSalsaString fieldName $ \fieldName' ->
            return $ getFieldGetStubRaw className' fieldName'

{-# NOINLINE getFieldGetStubRaw #-}
getFieldGetStubRaw :: GetFieldGetStubDelegate a
getFieldGetStubRaw = makeGetFieldGetStubDelegate $ unsafePerformIO $ unsafeGetPointerToMethod "GetFieldGetStub"

type GetFieldGetStubDelegate a = SalsaString -> SalsaString -> FunPtr a
foreign import ccall "dynamic" makeGetFieldGetStubDelegate :: FunPtr (GetFieldGetStubDelegate a) ->
    (GetFieldGetStubDelegate a)


-- | @'getFieldSetStub' c f@ returns a function pointer to a function that, when
--   called, sets the value of the field @f@ in class @c@ to the given value.
getFieldSetStub :: String -> String -> IO (FunPtr f)
getFieldSetStub className fieldName = do
    withSalsaString className $ \className' ->
        withSalsaString fieldName $ \fieldName' ->
            return $ getFieldSetStubRaw className' fieldName'

{-# NOINLINE getFieldSetStubRaw #-}
getFieldSetStubRaw :: GetFieldSetStubDelegate a
getFieldSetStubRaw = makeGetFieldSetStubDelegate $ unsafePerformIO $ unsafeGetPointerToMethod "GetFieldSetStub"

type GetFieldSetStubDelegate a = SalsaString -> SalsaString -> FunPtr a
foreign import ccall "dynamic" makeGetFieldSetStubDelegate :: FunPtr (GetFieldSetStubDelegate a) ->
    (GetFieldSetStubDelegate a)


-- | @'getDelegateConstructorStub' dt wrapper@ returns an action that, given a
--   function, will return a reference to a .NET delegate object that calls the
--   provided function.  The delegate constructed will be of the type @dt@.  
--   The function @wrapper@ will be called in order to wrap the given function
--   as a function pointer for passing into .NET.
getDelegateConstructorStub :: String -> (f -> IO (FunPtr f)) -> IO (f -> IO ObjectId)
getDelegateConstructorStub delegateTypeName wrapper = do
    -- Obtain a function pointer to a function that, when called with a
    -- function pointer compatible with the given wrapper function, returns
    -- a reference to a .NET delegate object that calls the function.
    delegateConstructor <- withSalsaString delegateTypeName $
        \delegateTypeName' -> getDelegateConstructorStubRaw delegateTypeName' 

    -- Returns a function that accepts a function, 'f' implementing the
    -- delegate, converts 'f' to a function pointer, and then wraps it
    -- up as a .NET delegate.
    return $ \f -> do
        fFunPtr <- wrapper f
        (makeDelegateConstructor delegateConstructor) fFunPtr

{-# NOINLINE getDelegateConstructorStubRaw #-}
getDelegateConstructorStubRaw :: GetDelegateConstructorStubDelegate a
getDelegateConstructorStubRaw = makeGetDelegateConstructorStubDelegate $ unsafePerformIO $ unsafeGetPointerToMethod "GetDelegateConstructorStub"

type GetDelegateConstructorStubDelegate a = SalsaString -> IO (FunPtr (FunPtr a -> IO ObjectId))
foreign import ccall "dynamic" makeGetDelegateConstructorStubDelegate :: FunPtr (GetDelegateConstructorStubDelegate a) ->
    (GetDelegateConstructorStubDelegate a)

type DelegateConstructor a = FunPtr a -> IO ObjectId
foreign import ccall "dynamic" makeDelegateConstructor :: FunPtr (DelegateConstructor a) -> (DelegateConstructor a)

--
-- Boxing support
--

-- | @'getBoxStub' t@ returns a function pointer to a function that, when
--   called, returns a boxed object reference to the given type.
getBoxStub :: String -> IO (FunPtr f)
getBoxStub typeName = do
    withSalsaString typeName $ \typeName' -> return $ getBoxStubRaw typeName'

{-# NOINLINE getBoxStubRaw #-}
getBoxStubRaw :: GetBoxStubDelegate a
getBoxStubRaw = makeGetBoxStubDelegate $ unsafePerformIO $ unsafeGetPointerToMethod "GetBoxStub"

type GetBoxStubDelegate a = SalsaString -> FunPtr a
foreign import ccall "dynamic" makeGetBoxStubDelegate :: FunPtr (GetBoxStubDelegate a) -> GetBoxStubDelegate a


boxString :: String -> IO ObjectId
boxString s = withSalsaString s $ \s' -> boxStringStub s'

type BoxStringStub = SalsaString -> IO ObjectId
foreign import ccall "dynamic" makeBoxStringStub :: FunPtr BoxStringStub -> BoxStringStub

{-# NOINLINE boxStringStub #-}
boxStringStub :: BoxStringStub
boxStringStub = makeBoxStringStub $ unsafePerformIO $ getBoxStub "System.String"


boxInt32 :: Int32 -> IO ObjectId
boxInt32 = boxInt32Stub

type BoxInt32Stub = Int32 -> IO ObjectId
foreign import ccall "dynamic" makeBoxInt32Stub :: FunPtr BoxInt32Stub -> BoxInt32Stub

{-# NOINLINE boxInt32Stub #-}
boxInt32Stub :: BoxInt32Stub
boxInt32Stub = makeBoxInt32Stub $ unsafePerformIO $ getBoxStub "System.Int32"


boxBoolean :: Bool -> ObjectId
boxBoolean True  = boxedTrue
boxBoolean False = boxedFalse

{-# NOINLINE boxedTrue #-}
boxedTrue  = unsafePerformIO $ boxBooleanStub True

{-# NOINLINE boxedFalse #-}
boxedFalse = unsafePerformIO $ boxBooleanStub False

type BoxBooleanStub = Bool -> IO ObjectId
foreign import ccall "dynamic" makeBoxBooleanStub :: FunPtr BoxBooleanStub -> BoxBooleanStub

{-# NOINLINE boxBooleanStub #-}
boxBooleanStub :: BoxBooleanStub
boxBooleanStub = makeBoxBooleanStub $ unsafePerformIO $ getBoxStub "System.Boolean"

-- vim:set ts=4 sw=4 expandtab:
