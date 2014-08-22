{-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses, EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, CPP, ForeignFunctionInterface, DataKinds, PolyKinds #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-----------------------------------------------------------------------------
module Foreign.Salsa.Core where

import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe ( unsafePerformIO )
import Foreign.C.String

import Foreign hiding (new, newForeignPtr, unsafePerformIO)
import Foreign.Concurrent (newForeignPtr)

import Foreign.Salsa.Common
import Foreign.Salsa.TypePrelude
import Foreign.Salsa.Resolver
import Foreign.Salsa.CLR

#if (MONO)
foreign import ccall mono_free :: Ptr a -> IO ()
localFree = mono_free
#else
import System.Win32
#endif

-- Reverse function application
x # f = f x

-- Binding to the target of a method invocation
t >>=# m = t >>= ( # m )

null_ :: Obj Null
null_ = ObjNull

isNull :: Obj a -> Bool
isNull ObjNull = True
isNull _       = False

--
-- Method invocation
--

-- | @'Candidates' t m@ is a type-level list of the members of a particular
--   method group, given the type @t@ and method name @m@.  The overload
--   resolution algorithm chooses the appropriate member of the method group
--   (from this list) according to the argument types.
type family Candidates t m

-- Calls 'ResolveMember', converting argument tuples to/from type-level lists,
-- and passing it the appropriate list of candidate signatures.
type family Resolve t m args where
    Resolve t m args = ListToTuple (ResolveMember (TupleToList args) (Candidates t m))

-- | 'Invoker' provides type-based dispatch to method implementations, and 
--   a constrainted result type for method invocations.
class Invoker t m args where 
    type Result t m args
    rawInvoke :: t -> m -> args -> Result t m args

-- | @'invoke' t m args@ invokes the appropriate method @m@ in type @t@ given
--  the tuple of arguments @args@.
invoke :: forall t m args args'.
          (Resolve t m args ~ args',
           Coercible args args',
           Invoker t m args') =>
           t -> m -> args -> Result t m args'
invoke t m args = rawInvoke t m (coerce args :: args')

--
-- Constructor invocation
--

data Ctor = Ctor deriving (Show, Eq)

-- | @'new' t args@ invokes the appropriate constructor for the type @t@
--   given the tuple of arguments @args@, and returns an instance of the
--   constructed type.
new t args = invoke t Ctor args

--
-- Delegate support
--

class Delegate dt where
    type DelegateT dt -- ^ High-level function type for the delegate implementation

    -- | @'delegate' dt h@ calls the constructor for the delegate of type @t@, returning
    --   a .NET delegate object of the same type whose implementation is the Haskell
    --   function, @h@.
    delegate :: dt -> (DelegateT dt) -> IO (Obj dt)


--
-- Attribute system for .NET properties and events
--

-- | @Prop t pn@ represents a .NET property with name @pn@, on the target object/class @t@.
class Prop t pn where
    type PropGT t pn -- ^ type of property when retrieved; () if write-only
    type PropST t pn -- ^ type of property when set; () if read-only
    setProp :: t -> pn -> PropST t pn -> IO ()
    getProp :: t -> pn -> IO (PropGT t pn)

-- | @Event t en@ represents a .NET event with name @pn@, on the target object/class @t@.
class Event t en where
    type EventT t en -- ^ type of the event delegate
    addEvent, removeEvent :: t -> en -> EventT t en -> IO ()

infix 0 :==, :=, :=>, :~, :+, :-, :+>, :->

-- | @AttrOp t@ represents a get/set operation on a .NET property, or an
--   add/remove operation on a .NET event, on a object/class of type @t@.
data AttrOp t
    = forall p. (Prop  t p) => p :== (PropST t p)               -- assign value to property (monotyped)
    | forall p v. (Prop t p, 
                   ConvertsTo v (PropST t p) ~ TTrue,
                   Coercible  v (PropST t p)) =>
                               p := v                           -- assign value to property (w/ implicit conversion)
    | forall p v. (Prop  t p,
                 ConvertsTo v (PropST t p) ~ TTrue,
                 Coercible  v (PropST t p)) =>
                               p :=> IO v                       -- assign result of IO action to property 
    | forall p. (Prop  t p) => p :~  (PropGT t p -> PropST t p) -- update property
    | forall e. (Event t e) => e :+  (EventT t e)               -- add event listener
    | forall e. (Event t e) => e :-  (EventT t e)               -- remove event listener
    | forall e. (Event t e) => e :+> IO (EventT t e)            -- add result of IO action to event listeners
    | forall e. (Event t e) => e :-> IO (EventT t e)            -- remove result of IO action from event listeners

-- | @'get' t p@ retrieves the value of property @p@ on the target object/class @t@.
get :: Prop t p => t -> p -> IO (PropGT t p)
get = getProp

-- | @'set' t ops@ applies the operations @ops@ to the object/class @t@.
set :: forall t. {-Target t =>-} t -> [AttrOp t] -> IO ()
set t ops = mapM_ applyOp ops
    where applyOp :: AttrOp t -> IO ()
          applyOp (p :== v) = setProp t p v
          applyOp (p :=  v) = setProp t p (coerce v)
          applyOp (p :=> v) = v >>= (\v -> setProp t p (coerce v))
          applyOp (p :~  f) = getProp t p >>= setProp t p . f
          applyOp (e :+  h) = h #   addEvent    t e
          applyOp (e :-  h) = h #   removeEvent t e
          applyOp (e :+> h) = h >>= addEvent    t e
          applyOp (e :-> h) = h >>= removeEvent t e

    -- 
    -- Note: a lexically scoped type variable is required in the definition of this
    -- function.  Both the 'forall t.' in the type signature for 'set' and the the
    -- type signature for 'applyOp' are required to ensure that the 't' variable
    -- used in 'set' is the same 't' as that used in 'applyOp'.  Without the scoped 
    -- type variable, GHC is unable to deduce the desired Prop instance for the call
    -- to 'setProp'.
    --


-- | 'TupleToList t' is the type-level list representation of the tuple @t@
--   containing marshalable types.  This allows arguments to .NET members to
--   be passed as tuples, which have a much neater syntax than lists.
type family TupleToList t where
    TupleToList (a,b,c,d,e)    = a ::: b ::: c ::: d ::: e ::: TNil
    TupleToList (a,b,c,d)      = a ::: b ::: c ::: d ::: TNil
    TupleToList (a,b,c)        = a ::: b ::: c ::: TNil
    TupleToList (a,b)          = a ::: b ::: TNil
    TupleToList ()             = TNil
    TupleToList (a)            = a ::: TNil
-- ...

-- | 'ListToTuple l' is the tuple type associated with the type-level list @l@.
type family ListToTuple t where
    ListToTuple (Error x)                            = Error x -- propagate errors
    ListToTuple TNil                                 = ()
    ListToTuple (a ::: TNil)                         = a
    ListToTuple (a ::: b ::: TNil)                   = (a,b) 
    ListToTuple (a ::: b ::: c ::: TNil)             = (a,b,c)
    ListToTuple (a ::: b ::: c ::: d ::: TNil)       = (a,b,c,d)
    ListToTuple (a ::: b ::: c ::: d ::: e ::: TNil) = (a,b,c,d,e)
-- ...

--
-- Type reflection (connects Haskell types to .NET types)
--

-- | The class 'SalsaForeignType' provides access to the underlying .NET type that is 
--   associated with a given Haskell type.
class SalsaForeignType t where 
    -- | Returns the .NET System.Type instance for values of type 't'.
    --   The value of 't' is not evaluated by the function.
    foreignTypeOf :: t -> Obj Type_



--
-- Value coercion
--

-- | @'Coercible' from to@ provides a function 'coerce' for implicitly converting
--   values of type @from@ to @to@.  It applies only to high-level bridge types
--   (low-level conversions are handled by the marshaling system).  It always
--   succeeds.
class Coercible from to where
    -- | @'coerce' v@ returns the value @v@ implicitly converted to the desired type.
    coerce :: from -> to

instance Coercible Int32   Int32   where coerce = id
instance Coercible String  String  where coerce = id
instance Coercible Bool    Bool    where coerce = id
instance Coercible Double  Double  where coerce = id
instance Coercible Int32   Double  where coerce = fromIntegral
instance Coercible (Obj f) (Obj t) where coerce = unsafeCoerce

instance Coercible String  (Obj t) where
    coerce s = unsafePerformIO (boxString s >>= unmarshal) -- boxing conversion
    -- TODO: Ensure that this is sufficiently referentially transparent.

instance Coercible Int32  (Obj t) where
    coerce i = unsafePerformIO (boxInt32 i >>= unmarshal) -- boxing conversion
    -- TODO: Ensure that this is sufficiently referentially transparent.

-- Coercible tuples:
instance Coercible ()      ()      where coerce = id
instance (Coercible f0 t0, Coercible f1 t1) =>
    Coercible (f0,f1) (t0,t1) where
    coerce (f0,f1) = (coerce f0, coerce f1)
instance (Coercible f0 t0, Coercible f1 t1, Coercible f2 t2) =>
    Coercible (f0,f1,f2) (t0,t1,t2) where
    coerce (f0,f1,f2) = (coerce f0, coerce f1, coerce f2)
-- ...

-- Lift coerce into the IO monad
instance (Coercible f t) => Coercible f (IO t)  where 
    coerce = return . coerce

cast v = coerce v

-- Checked implicit conversion:
convert :: (Coercible from to, ConvertsTo from to ~ TTrue) => from -> to
convert v = coerce v

-- TODO: Fix up the whole: coerce vs. cast vs. convert thing.
--       Work out what's required (i.e. implicit/explicit conversions,
--       checked or unchecked, etc.)

--
-- Marshaling support
--

-- For converting between Haskell types and the proxy types used for
-- communicating with .NET.

-- | The class 'Marshal' allows high-level Haskell types to be converted into 
--   low-level types when calling into FFI functions.
class Marshal from to where
    marshal :: from -> (to -> IO a) -> IO a

instance Marshal String SalsaString where
    marshal s = withSalsaString s

instance Marshal (Obj a) ObjectId where
    -- | @'marshal' o k@ provides access to the object identifier in @o@ within
    --   the IO action @k@, ensuring that the .NET object instance is kept alive
    --   during the action (like 'withForeignPtr' except for .NET objects).
    marshal (Obj oId fp) k = withForeignPtr fp (\_ -> k oId)
    marshal ObjNull      k = k 0

instance Marshal Int Int32 where
    marshal v f = f (toEnum v)

instance Marshal ()     ()     where marshal = ( # )
instance Marshal String String where marshal = ( # )
instance Marshal Int32  Int32  where marshal = ( # )
instance Marshal Bool   Bool   where marshal = ( # )
instance Marshal Double Double where marshal = ( # )

-- Special case for Nullable<Boolean>
instance Marshal (Maybe Bool) ObjectId where
    marshal Nothing      k = k 0 
    marshal (Just True)  k = k 1
    marshal (Just False) k = k 2


-- | The class 'Unmarshal' allows low-level types returned by FFI functions to
--   to be converted into high-level Haskell types.
class Unmarshal from to where
    unmarshal :: from -> IO to

instance Unmarshal a () where
    unmarshal _ = return ()

instance Unmarshal ObjectId (Obj a) where
    -- | @'unmarshal' o@ wraps the .NET object identified by @o@ as an Obj value.
    -- 'releaseObject' is called with the object identifier when the Obj is
    -- garbage collected.
    unmarshal 0   = return ObjNull
    unmarshal oId = newForeignPtr nullPtr (releaseObject oId) >>= return . Obj oId

instance Unmarshal SalsaString String where
    unmarshal s = do
        s' <- peekSalsaString s
        localFree s -- Free the string allocated by the .NET marshaler (use
                    -- LocalFree for LPWSTRs and SysFreeString for BSTRs)
        return s'

instance Unmarshal String String where unmarshal = return
instance Unmarshal Int32  Int32  where unmarshal = return
instance Unmarshal Bool   Bool   where unmarshal = return
instance Unmarshal Double Double where unmarshal = return

-- Special case for Nullable<Boolean>
instance Unmarshal ObjectId (Maybe Bool) where
    unmarshal id | id == 1 = return $ Just True
                 | id == 2 = return $ Just False
                 | id == 0 = return $ Nothing
                 | otherwise = error "unmarshal: unexpected ObjectId for Maybe Bool"


-- TODO: Generate the functions below using Template Haskell (this code is hideous).

-- Generic marshaling functions for instance methods
-- (marshal the target object and the tuple of arguments then call the given
--  function and unmarshal the result)
marshalMethod0i f o _ ()        = marshal o (\o' -> f o') >>= unmarshal
marshalMethod1i f o _ (a)       = marshal o (\o' -> marshal a (\a' -> f o' a')) >>= unmarshal
marshalMethod2i f o _ (a,b)     = marshal o (\o' -> marshal a (\a' -> marshal b (\b' -> f o' a' b'))) >>= unmarshal
marshalMethod3i f o _ (a,b,c)   = marshal o (\o' -> marshal a (\a' -> marshal b (\b' -> marshal c (\c' -> f o' a' b' c')))) >>= unmarshal
marshalMethod4i f o _ (a,b,c,d) =
    marshal o (\o' -> marshal a (\a' -> marshal b (\b' -> marshal c (\c' ->
    marshal d (\d' -> f o' a' b' c' d'))))) >>= unmarshal
marshalMethod5i f o _ (a1,a2,a3,a4,a5) =
    marshal o (\o'   -> marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' ->
    marshal a4 (\a4' -> marshal a5 (\a5' -> f o' a1' a2' a3' a4' a5')))))) >>= unmarshal
marshalMethod6i f o _ (a1,a2,a3,a4,a5,a6) =
    marshal o (\o'   -> marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' ->
    marshal a4 (\a4' -> marshal a5 (\a5' -> marshal a6 (\a6' -> 
    f o' a1' a2' a3' a4' a5' a6'))))))) >>= unmarshal
marshalMethod7i f o _ (a1,a2,a3,a4,a5,a6,a7) =
    marshal o (\o'   -> marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' ->
    marshal a4 (\a4' -> marshal a5 (\a5' -> marshal a6 (\a6' -> marshal a7 (\a7' ->
    f o' a1' a2' a3' a4' a5' a6' a7')))))))) >>= unmarshal
marshalMethod8i f o _ (a1,a2,a3,a4,a5,a6,a7,a8) =
    marshal o (\o'   -> marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' ->
    marshal a4 (\a4' -> marshal a5 (\a5' -> marshal a6 (\a6' -> marshal a7 (\a7' ->
    marshal a8 (\a8' ->
    f o' a1' a2' a3' a4' a5' a6' a7' a8'))))))))) >>= unmarshal
marshalMethod9i f o _ (a1,a2,a3,a4,a5,a6,a7,a8,a9) =
    marshal o (\o'   -> marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' ->
    marshal a4 (\a4' -> marshal a5 (\a5' -> marshal a6 (\a6' -> marshal a7 (\a7' ->
    marshal a8 (\a8' -> marshal a9 (\a9' ->
    f o' a1' a2' a3' a4' a5' a6' a7' a8' a9')))))))))) >>= unmarshal
marshalMethod10i f o _ (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) =
    marshal o   (\o'   -> marshal a1  (\a1'  -> marshal a2  (\a2'  -> marshal a3  (\a3'  ->
    marshal a4  (\a4'  -> marshal a5  (\a5'  -> marshal a6  (\a6'  -> marshal a7  (\a7'  ->
    marshal a8  (\a8'  -> marshal a9  (\a9'  -> marshal a10 (\a10' ->
    f o' a1' a2' a3' a4' a5' a6' a7' a8' a9' a10'))))))))))) >>= unmarshal
marshalMethod11i f o _ (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) =
    marshal o   (\o'   -> marshal a1  (\a1'  -> marshal a2  (\a2'  -> marshal a3  (\a3'  ->
    marshal a4  (\a4'  -> marshal a5  (\a5'  -> marshal a6  (\a6'  -> marshal a7  (\a7'  ->
    marshal a8  (\a8'  -> marshal a9  (\a9'  -> marshal a10 (\a10' -> marshal a11 (\a11' ->
    f o' a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11')))))))))))) >>= unmarshal
-- ...

-- Generic marshaling functions for static methods
-- (marshal the tuple of arguments then call the given function and unmarshal
--  the result)
marshalMethod0s f _ _ ()        = f >>= unmarshal
marshalMethod1s f _ _ (a)       = marshal a (\a' -> f a') >>= unmarshal
marshalMethod2s f _ _ (a,b)     = marshal a (\a' -> marshal b (\b' -> f a' b')) >>= unmarshal
marshalMethod3s f _ _ (a,b,c)   = marshal a (\a' -> marshal b (\b' -> marshal c (\c' -> f a' b' c'))) >>= unmarshal
marshalMethod4s f _ _ (a,b,c,d) =
    marshal a (\a' -> marshal b (\b' -> marshal c (\c' -> marshal d (\d' ->
    f a' b' c' d')))) >>= unmarshal
marshalMethod5s f _ _ (a,b,c,d,e) =
    marshal a (\a' -> marshal b (\b' -> marshal c (\c' -> marshal d (\d' ->
    marshal e (\e' -> f a' b' c' d' e'))))) >>= unmarshal
marshalMethod6s f _ _ (a1,a2,a3,a4,a5,a6) =
    marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' -> marshal a4 (\a4' ->
    marshal a5 (\a5' -> marshal a6 (\a6' -> f a1' a2' a3' a4' a5' a6')))))) >>= unmarshal
marshalMethod7s f _ _ (a1,a2,a3,a4,a5,a6,a7) =
    marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' -> marshal a4 (\a4' ->
    marshal a5 (\a5' -> marshal a6 (\a6' -> marshal a7 (\a7' ->
    f a1' a2' a3' a4' a5' a6' a7'))))))) >>= unmarshal
marshalMethod8s f _ _ (a1,a2,a3,a4,a5,a6,a7,a8) =
    marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' -> marshal a4 (\a4' ->
    marshal a5 (\a5' -> marshal a6 (\a6' -> marshal a7 (\a7' -> marshal a8 (\a8' ->
    f a1' a2' a3' a4' a5' a6' a7' a8')))))))) >>= unmarshal
marshalMethod9s f _ _ (a1,a2,a3,a4,a5,a6,a7,a8,a9) =
    marshal a1 (\a1' -> marshal a2 (\a2' -> marshal a3 (\a3' -> marshal a4 (\a4' ->
    marshal a5 (\a5' -> marshal a6 (\a6' -> marshal a7 (\a7' -> marshal a8 (\a8' ->
    marshal a9 (\a9' ->
    f a1' a2' a3' a4' a5' a6' a7' a8' a9'))))))))) >>= unmarshal
marshalMethod10s f _ _ (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) =
    marshal a1  (\a1'  -> marshal a2  (\a2'  -> marshal a3  (\a3'  -> marshal a4  (\a4'  ->
    marshal a5  (\a5'  -> marshal a6  (\a6'  -> marshal a7  (\a7'  -> marshal a8  (\a8'  ->
    marshal a9  (\a9'  -> marshal a10 (\a10' ->
    f a1' a2' a3' a4' a5' a6' a7' a8' a9' a10')))))))))) >>= unmarshal
marshalMethod11s f _ _ (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) =
    marshal a1  (\a1'  -> marshal a2  (\a2'  -> marshal a3  (\a3'  -> marshal a4  (\a4'  ->
    marshal a5  (\a5'  -> marshal a6  (\a6'  -> marshal a7  (\a7'  -> marshal a8  (\a8'  ->
    marshal a9  (\a9'  -> marshal a10 (\a10' -> marshal a11 (\a11' ->
    f a1' a2' a3' a4' a5' a6' a7' a8' a9' a10' a11'))))))))))) >>= unmarshal
-- ...

-- Converts a function accepting and returning high-level types, to a function
-- that takes and returns low-level (base) types.

marshalFn0 f = do
    o <- f
    oId <- marshal o return
    return oId

marshalFn1 f = \a1' -> unmarshal a1' >>= (\a1 -> f a1 >>= flip marshal return)

marshalFn2 f = \f1 f2 -> do
    t1 <- unmarshal f1
    t2 <- unmarshal f2
    r <- f t1 t2
    --
    -- FIXME: We might have an issue here when 'r' is an object (Obj).  After
    -- marshal returns, it is possible that no references to 'r' remain in
    -- Haskell.  If the finalizer for the object is called before the 
    -- identifier is returned to .NET and looked up in the in-table, then
    -- the lookup will fail.  The following code forces this error:
    --
    --   marshal r return >>= (\rId -> performGC >> yield >> return rId)
    --
    marshal r return

marshalFn3 f = \a1' a2' a3' -> 
    unmarshal a1' >>= (\a1 -> unmarshal a2' >>= (\a2 -> unmarshal a3' >>= (\a3 ->
    (f a1 a2 a3 >>= flip marshal return))))

marshalFn4 f = \a1' a2' a3' a4' -> 
    unmarshal a1' >>= (\a1 -> unmarshal a2' >>= (\a2 -> unmarshal a3' >>= (\a3 ->
    unmarshal a4' >>= (\a4 -> (f a1 a2 a3 a4 >>= flip marshal return)))))

marshalFn5 f = \a1' a2' a3' a4' a5' -> do
    a1 <- unmarshal a1'
    a2 <- unmarshal a2'
    a3 <- unmarshal a3'
    a4 <- unmarshal a4'
    a5 <- unmarshal a5'
    v <- f a1 a2 a3 a4 a5
    return $ marshal v
    {- 
    >>= (\a1 -> unmarshal a2' >>= (\a2 -> unmarshal a3' >>= (\a3 ->
    unmarshal a4' >>= (\a4 -> unmarshal a5' >>= (\a5 ->
    (f a1 a2 a3 a5 a5 >>= flip marshal return))))))
    -}

-- ...

-- vim:set sw=4 ts=4 expandtab:
