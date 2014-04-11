{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Windows specific bootstapping and exports.
-- Exposes some of the methods of the ICLRRuntimeHost COM interface, which
-- can be used to host the the Microsoft CLR in the process, and to execute
-- code from a .NET assembly.  Includes basic functionality for dealing
-- with the Microsoft COM.
--
-----------------------------------------------------------------------------
module Foreign.Salsa.Win.CLRHost (
    startCLR',
    stopCLR',
    loadDriverAndBoot,
    SalsaString, withSalsaString, peekSalsaString
    ) where

import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.String
import System.Win32
import System.IO
import Control.Exception (bracket)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as S
import Text.Printf

import Foreign.Salsa.Driver

--
-- Global static functions for hosting the CLR
--

type ICorRuntimeHost = InterfacePtr



-- | 'clrHost' stores a reference to the ICLRRuntimeHost for the .NET execution
--   engine that is hosted in the process.
{-# NOINLINE clrHost #-}
clrHost :: ICorRuntimeHost
clrHost = unsafePerformIO $ corBindToRuntimeEx

startCLR' = start_ICorRuntimeHost clrHost
stopCLR' = stop_ICorRuntimeHost clrHost


-- | 'corBindToRunTimeEx' loads the CLR execution engine into the process and returns
--   a COM interface for it.
corBindToRuntimeEx :: IO ICorRuntimeHost
corBindToRuntimeEx = do
    -- Load the 'mscoree' dynamic library into the process.  This is the
    -- 'stub' library for the .NET execution engine, and is used to load an
    -- appropriate version of the real runtime via a call to
    -- 'CorBindToRuntimeEx'.
    hMscoree <- loadLibrary "mscoree.dll"

    -- Obtain a pointer to the 'CorBindToRuntimeEx' function from mscoree.dll
    corBindToRuntimeExAddr <- getProcAddress hMscoree "CorBindToRuntimeEx"
    let corBindToRuntimeEx = makeCorBindToRuntimeEx $ castPtrToFunPtr corBindToRuntimeExAddr

    let clsid_CorRuntimeHost = Guid 0xCB2F6723 0xAB3A 0x11D2 0x9C 0x40 0x00 0xC0 0x4F 0xA3 0x0A 0x3E
        iid_ICorRuntimeHost  = Guid 0xCB2F6722 0xAB3A 0x11D2 0x9C 0x40 0x00 0xC0 0x4F 0xA3 0x0A 0x3E

    -- Request the shim (mscoree.dll) to load a version of the runtime into the
    -- process, returning a pointer to an implementation of the ICorRuntimeHost
    -- for controlling the runtime.
    with (nullPtr :: ICorRuntimeHost) $ \clrHostPtr -> do
        -- Call 'corBindToRuntimeEx' to obtain an ICorRuntimeHost 
        with clsid_CorRuntimeHost $ \refCLSID_CorRuntimeHost -> 
            with iid_ICorRuntimeHost $ \refIID_ICorRuntimeHost -> 
                corBindToRuntimeEx nullPtr nullPtr 0 refCLSID_CorRuntimeHost
                    refIID_ICorRuntimeHost clrHostPtr >>= checkHR "CorBindToRuntimeEx"
        peek clrHostPtr

type CorBindToRuntimeEx = LPCWSTR -> LPCWSTR -> DWORD -> Ptr CLSID -> Ptr IID -> Ptr ICorRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeCorBindToRuntimeEx :: FunPtr CorBindToRuntimeEx -> CorBindToRuntimeEx


-- | 'start_ICorRuntimeHost' calls the Start method of the given ICorRuntimeHost interface.
start_ICorRuntimeHost this = do
    -- Initialise COM (and the threading model)
    coInitializeEx nullPtr coInit_ApartmentThreaded
    -- TODO: Allow the library user to select their desired threading model
    --       (we use an STA for the time being so we can use GUI libraries).

    f <- getInterfaceFunction 10 makeStart this
    f this >>= checkHR "ICorRuntimeHost.Start"

type Start = ICorRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeStart :: FunPtr Start -> Start


-- | 'stop_ICorRuntimeHost' calls the Stop method of the given ICorRuntimeHost interface.
stop_ICorRuntimeHost this = do
    f <- getInterfaceFunction 11 makeStop this
    f this >>= checkHR "ICorRuntimeHost.Stop"

    coUninitialize

type Stop = ICorRuntimeHost -> IO HResult
foreign import stdcall "dynamic" makeStop :: FunPtr Stop -> Stop


-- | 'getDefaultDomain_ICorRuntimeHost' calls the GetDefaultDOmain method of the given
--   ICorRuntimeHost interface.
getDefaultDomain_ICorRuntimeHost this = do
    f <- getInterfaceFunction 13 makeGetDefaultDomain this
    with (nullPtr :: InterfacePtr) $ \appDomainPtr -> do
        f this appDomainPtr >>= checkHR "ICorRuntimeHost.GetDefaultDomain"
        peek appDomainPtr

type GetDefaultDomain = ICorRuntimeHost -> Ptr InterfacePtr -> IO HResult
foreign import stdcall "dynamic" makeGetDefaultDomain :: FunPtr GetDefaultDomain -> GetDefaultDomain


type AppDomain = InterfacePtr -- mscorlib::_AppDomain

-- | 'load_AppDomain' calls mscorlib::_AppDomain.Load_3(SafeArray* rawAssembly, _Assembly** result).
load_AppDomain this rawAssembly = do
    f <- getInterfaceFunction 45 makeLoad_AppDomain this
    with (nullPtr :: Assembly) $ \assemblyPtr -> do
        f this rawAssembly assemblyPtr >>= checkHR "AppDomain.Load"
        peek assemblyPtr

type Load_AppDomain = AppDomain -> SafeArray -> Ptr Assembly -> IO HResult
foreign import stdcall "dynamic" makeLoad_AppDomain :: FunPtr Load_AppDomain -> Load_AppDomain

-- | 'load_AppDomain_2' calls mscorlib::_AppDomain.Load_2(BStr assemblyString, _Assembly** result).
load_AppDomain_2 this assemblyString = do
    f <- getInterfaceFunction 44 makeLoad_AppDomain_2 this
    withBStr assemblyString $ \assemblyString' -> do
        with (nullPtr :: InterfacePtr) $ \assemblyPtr -> do
            f this assemblyString' assemblyPtr >>= checkHR "AppDomain.Load"
            peek assemblyPtr

type Load_AppDomain_2 = AppDomain -> BStr -> Ptr InterfacePtr -> IO HResult
foreign import stdcall "dynamic" makeLoad_AppDomain_2 :: FunPtr Load_AppDomain_2 -> Load_AppDomain_2


type Assembly = InterfacePtr -- mscorlib::_Assembly

-- | 'getType_Assembly' calls mscorlib::_Assembly.GetType_2(BStr name, _Type** result).
getType_Assembly this name = do
    f <- getInterfaceFunction 17 makeGetType_Assembly this
    withBStr name $ \name' ->
        with (nullPtr :: Type) $ \typePtr -> do
            f this name' typePtr >>= checkHR "Assembly.GetType"
            t <- peek typePtr
            if t == nullPtr then error "Assembly.GetType failed"
                            else return t

type GetType_Assembly = Assembly -> BStr -> Ptr Type -> IO HResult
foreign import stdcall "dynamic" makeGetType_Assembly :: FunPtr GetType_Assembly -> GetType_Assembly


type Type = InterfacePtr -- mscorlib::_Type

-- | 'invokeMember_Type' calls mscorlib::_Type.InvokeMember_3(BStr name,
--   BindingFlags invokeAttr, _Binder* binder, Variant target, SafeArray* args,
--   Variant* result) to invoke a method without a binder, target or any arguments.
invokeMember_Type this memberName = do
    f <- getInterfaceFunction 57 {- _Type.InvokeMember_3 -} makeInvokeMember_Type this
    withBStr memberName $ \memberName' ->
        with emptyVariant $ \resultPtr -> do
            f this memberName' 256 {- BindingFlags.InvokeMethod -}
                nullPtr 0 0 nullPtr resultPtr >>= checkHR "Type.InvokeMember"
            peek resultPtr

-- Portability note: Type.InvokeMember accepts a by-value variant argument (as its fourth
--                   argument).  In the declaration below, this is encoded as two Word64
--                   arguments.
type InvokeMember_Type = Type -> BStr -> Word32 -> Ptr () -> Word64 -> Word64 -> Ptr () -> Ptr Variant -> IO HResult
foreign import stdcall "dynamic" makeInvokeMember_Type :: FunPtr InvokeMember_Type -> InvokeMember_Type


-- | 'loadDriverAndBoot' loads the Salsa driver assembly into the application domain from 
--   memory (the binary data is originally stored in 'driverData'), and then invokes the
--   Boot method (from the Salsa.Driver class) to obtain a function pointer for invoking 
--   the 'GetPointerToMethod' method.
loadDriverAndBoot' :: ICorRuntimeHost -> IO (FunPtr (SalsaString -> IO (FunPtr a)))
loadDriverAndBoot' clrHost = do
    -- Obtain an _AppDomain interface pointer to the default application domain
    withInterface (getDefaultDomain_ICorRuntimeHost clrHost) $ \untypedAppDomain -> do
        let iid_AppDomain = Guid 0x05F696DC 0x2B29 0x3663 0xAD 0x8B 0xC4 0x38 0x9C 0xF2 0xA7 0x13
        withInterface (queryInterface untypedAppDomain iid_AppDomain) $ \appDomain -> do

            -- Create a safe array for the contents of the driver assembly binary
            bracket (prim_SafeArrayCreateVector varType_UI1 0 (fromIntegral $ S.length driverData))
                    (prim_SafeArrayDestroy)
                (\sa -> do
                    -- Copy the driver assembly data into the safe array
                    saDataPtr <- with (nullPtr :: Ptr Word8) $ \ptr ->
                                      prim_SafeArrayAccessData sa ptr >> peek ptr
                    S.useAsCStringLen driverData $ \(bsPtr,bsLen) -> do
                        copyBytes saDataPtr (unsafeCoerce bsPtr) bsLen 
                    prim_SafeArrayUnaccessData sa

                    -- Load the driver assembly into the application domain
                    withInterface (load_AppDomain appDomain sa) $ \assembly -> do

                        -- Obtain a _Type interface pointer to the Salsa.Driver type
                        withInterface (getType_Assembly assembly "Salsa.Driver") $ \typ -> do

                            -- Invoke the Boot method of Salsa.Driver to obtain a function
                            -- pointer to the 'GetPointerToMethod' method
                            (Variant _ returnValue) <- invokeMember_Type typ "Boot"

                            -- Return a wrapper function for the 'GetPointerToMethod' method
                            return $ unsafeCoerce returnValue)

loadDriverAndBoot = loadDriverAndBoot' clrHost



{-

-- | 'executeInDefaultAppDomain_ICLRRuntimeHost' calls the ExecuteInDefaultAppDomain
--   method of the given ICLRRuntimeHost interface.
executeInDefaultAppDomain_ICLRRuntimeHost :: ICLRRuntimeHost -> String -> String -> String -> String -> IO DWORD
executeInDefaultAppDomain_ICLRRuntimeHost this assemblyPath typeName methodName argument = do
    f <- getInterfaceFunction 11 makeExecuteInDefaultAppDomain this
    with (0 :: DWORD) $ \resultPtr -> do
        hResult <- withCWString assemblyPath $ \assemblyPath' -> 
                       withCWString typeName $ \typeName' ->
                           withCWString methodName $ \methodName' ->
                               withCWString argument $ \argument' ->
                                   f this assemblyPath' typeName' methodName' argument' resultPtr
        peek resultPtr

type ExecuteInDefaultAppDomain = ICLRRuntimeHost -> LPCWSTR -> LPCWSTR -> LPCWSTR -> LPCWSTR -> Ptr DWORD -> IO HResult
foreign import stdcall "dynamic" makeExecuteInDefaultAppDomain :: FunPtr ExecuteInDefaultAppDomain -> ExecuteInDefaultAppDomain

-}

--
-- Types and functions for programming the COM
--

type HResult  = Word32
type CLSID    = Guid
type IID      = Guid

-- | 'InterfacePtr' is a pointer to an arbitrary COM interface (which is a pointer to
--   a vtable of function pointers for the interface methods).
type InterfacePtr = Ptr (Ptr (FunPtr ()))


-- | 'queryInterface' calls the QueryInterface method of the given COM interface.
queryInterface this iid = do
    f <- getInterfaceFunction 0 {- QueryInterface -} makeQueryInterface this
    with (nullPtr :: InterfacePtr) $ \interfacePtr -> do
        with iid $ \refIID -> f this refIID interfacePtr >>= checkHR "IUnknown.QueryInterface"
        peek interfacePtr

type QueryInterface = InterfacePtr -> Ptr IID -> Ptr InterfacePtr -> IO HResult
foreign import stdcall "dynamic" makeQueryInterface :: FunPtr QueryInterface -> QueryInterface


-- | 'release' calls the Release method of the given COM interface.
release this = do
    f <- getInterfaceFunction 2 {- Release -} makeRelease this
    f this

type Release = InterfacePtr -> IO Word32
foreign import stdcall "dynamic" makeRelease :: FunPtr Release -> Release


-- | 'withInterface i f' is like 'bracket' but for COM interface pointers, it calls
--   'release' once the computation is finished.
withInterface :: (IO InterfacePtr) -> (InterfacePtr -> IO a) -> IO a
withInterface i = bracket i (\x -> if x == nullPtr then return 0 else release x)


-- | 'getInterfaceFunction' @i makeFun obj@ is an action that returns the @i@th function
--   of the COM interface referred to by @obj@.  The function is returned as a Haskell
--   function by passing it through @makeFun@.
getInterfaceFunction :: Int -> (FunPtr a -> b) -> InterfacePtr -> IO b
getInterfaceFunction index makeFun this = do
    -- Obtain a pointer to the appropriate element in the vtable for this interface
    funPtr <- peek this >>= (flip peekElemOff) index
    -- Cast the function pointer to the expected type, and import it as a Haskell function
    return $ makeFun $ castFunPtr funPtr


foreign import stdcall "CoInitializeEx" coInitializeEx :: Ptr () -> Int32 -> IO HResult
foreign import stdcall "CoUninitialize" coUninitialize :: IO ()

coInit_MultiThreaded     = 0 :: Int32
coInit_ApartmentThreaded = 2 :: Int32

data Guid = Guid Word32 Word16 Word16 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 
    deriving (Show, Eq)

instance Storable Guid where
    sizeOf    _ = 16
    alignment _ = 4

    peek guidPtr = do
        a  <- peek $ plusPtr guidPtr 0 
        b  <- peek $ plusPtr guidPtr 4
        c  <- peek $ plusPtr guidPtr 6
        d0 <- peek $ plusPtr guidPtr 8
        d1 <- peek $ plusPtr guidPtr 9
        d2 <- peek $ plusPtr guidPtr 10
        d3 <- peek $ plusPtr guidPtr 11
        d4 <- peek $ plusPtr guidPtr 12
        d5 <- peek $ plusPtr guidPtr 13
        d6 <- peek $ plusPtr guidPtr 14
        d7 <- peek $ plusPtr guidPtr 15
        return $ Guid a b c d0 d1 d2 d3 d4 d5 d6 d7

    poke guidPtr (Guid a b c d0 d1 d2 d3 d4 d5 d6 d7) = do
        poke (plusPtr guidPtr 0)  a
        poke (plusPtr guidPtr 4)  b
        poke (plusPtr guidPtr 6)  c
        poke (plusPtr guidPtr 8)  d0
        poke (plusPtr guidPtr 9)  d1
        poke (plusPtr guidPtr 10) d2
        poke (plusPtr guidPtr 11) d3
        poke (plusPtr guidPtr 12) d4
        poke (plusPtr guidPtr 13) d5
        poke (plusPtr guidPtr 14) d6
        poke (plusPtr guidPtr 15) d7

--
-- Variant Support
--

data Variant = Variant VarType Word64 deriving (Show, Eq)
type VarType = Word16

varType_Empty, varType_UI1 :: VarType
varType_Empty = 0
varType_UI1   = 17

emptyVariant = Variant varType_Empty 0

instance Storable Variant where
    sizeOf    _ = 16
    alignment _ = 8

    peek ptr = do
        a  <- peek $ plusPtr ptr 0 
        b  <- peek $ plusPtr ptr 8
        return $ Variant a b

    poke ptr (Variant a b) = do
        poke (plusPtr ptr 0) a
        poke (plusPtr ptr 2) (0 :: Word16)
        poke (plusPtr ptr 4) (0 :: Word16)
        poke (plusPtr ptr 6) (0 :: Word16)
        poke (plusPtr ptr 8) b

--
-- Safe Array Support
--

newtype SafeArray = SafeArray (Ptr ()) deriving (Show, Eq)

foreign import stdcall "SafeArrayCreateVector" prim_SafeArrayCreateVector :: VarType -> Int32 -> Word32 -> IO SafeArray
foreign import stdcall "SafeArrayAccessData"   prim_SafeArrayAccessData   :: SafeArray -> Ptr (Ptr a) -> IO HResult
foreign import stdcall "SafeArrayUnaccessData" prim_SafeArrayUnaccessData :: SafeArray -> IO HResult 
foreign import stdcall "SafeArrayDestroy"      prim_SafeArrayDestroy      :: SafeArray -> IO HResult

--
-- BStr Support
--

newtype BStr = BStr (Ptr ()) deriving (Show, Eq)

sysAllocString :: String -> IO BStr
sysAllocString s = withCWString s prim_SysAllocString
foreign import stdcall "oleauto.h SysAllocString" prim_SysAllocString :: CWString -> IO BStr

sysFreeString :: BStr -> IO ()
sysFreeString = prim_SysFreeString
foreign import stdcall "oleauto.h SysFreeString" prim_SysFreeString :: BStr -> IO ()

withBStr :: String -> (BStr -> IO a) -> IO a
withBStr s = bracket (sysAllocString s) (sysFreeString)

--
-- HResult Support
--

checkHR :: String -> HResult -> IO HResult
checkHR msg 0 = return 0
checkHR msg r = error $ printf "%s failed (0x%8x)" msg r


type SalsaString = CWString
withSalsaString = withCWString
peekSalsaString = peekCWString

-- vim:set ts=4 sw=4 expandtab:
