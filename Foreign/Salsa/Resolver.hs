{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, EmptyDataDecls, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, DataKinds, PolyKinds, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
--
-- Contains a type-level implementation of the C# function member overload
-- resolution algorithm, as described in the \"C# Language Specification\"
-- (particularly, section 7.4.2).
--
-----------------------------------------------------------------------------
module Foreign.Salsa.Resolver where

import Data.Type.Bool
import Data.Type.Equality

import Foreign.Salsa.Common

--
-- Basic predicates
--

type family    IsPrim t :: Bool
type instance  IsPrim Int32      = True
type instance  IsPrim String     = True
type instance  IsPrim Bool       = True
type instance  IsPrim Double     = True
type instance  IsPrim (Obj t)    = False

type family    IsRef t :: Bool
type instance  IsRef t = Not (IsPrim t)
-- FIXME: This definition of IsRef is incorrect.  String is a reference
--        type (in .NET) but it is also a primitive bridge type.

-- | @'TyElem' t ts@ is true iff the type @t@ is present in the list @ts@
--   (containing coded type representations).
type family    TyElem (t1 :: *) (ts::[*]) :: Bool where
    TyElem t1 '[]        = False
    TyElem t1 (t ': ts)  = (t1 == t) || (TyElem t1 ts)

-- | @'FromSingleton' xs def@ returns the only element of the list @xs@ (if it is a
--   single element list), otherwise it returns the default value @def@.
type family FromSingleton (def :: k) (xs::[k]) :: k where
    FromSingleton def (x ': '[])    = x
    FromSingleton def x           = def

--
-- Overload resolution algorithm:
--

-- | @'ResolveMember' ms as@ returns the best applicable function member from @ms@
--   with respect to the argument list @as@ (if there is exactly one such member)
--   (ref 7.4.2).
--
--     resolveMember :: [Member] -> [Type] -> Member
--     resolveMember ms as =
--         case (filterBestMembers as applicables applicables) of
--             [m] -> m
--             _   -> error "Ambiguous or no match"
--         where applicables = filterApplicables as ms
--
type family ResolveMember (as::[*]) (ms::[[*]]) :: [*] where
  ResolveMember as ms = ResolveMember' as (FilterApp as ms)
-- was: FromSingleton (Error NoMatch) (FilterBestMembers as (FilterApp as ms) (FilterApp as ms))

type family ResolveMember' (as::[*]) (fa::[[*]]) :: [*] where
  ResolveMember' as fa = FromSingleton ((Error NoMatch) ': '[]) (FilterBestMembers as fa fa)


data Error x
data NoMatch


-- | @'FilterBestMembers' as ms ms@ returns the list of members from @ms@ that are
--   better than all the other members in @ms@ (with respect to the argument list
--   @as@). 
--
--     filterBestMembers :: [Type] -> [Member] -> [Member] -> [Member]
--     filterBestMembers as ms ns = filter (isBestMember as ms) ns
--
type family FilterBestMembers (as::[*]) (ms::[[*]]) (ns::[[*]]) :: [[*]] where
    FilterBestMembers as ms '[]       = '[]
    FilterBestMembers as ms (n ': ns) = FilterBestMembers' as ms n (FilterBestMembers as ms ns)
-- was: If (IsBestMember as ms n) (n ': (FilterBestMembers as ms ns))
--                                (FilterBestMembers as ms ns)

type family FilterBestMembers' (as::[*]) (ms::[[*]]) (n :: [*] ) (fbms::[[*]]) :: [[*]] where
    FilterBestMembers' as ms n fbms = If (IsBestMember as ms n) (n ': fbms) fbms

-- | @'IsBestMember' as ms n@ is true iff the member @n@ is better than all
--   the (other) members in @ms@ with respect to the argument list @as@. 
--
--     isBestMember :: [Type] -> [Member] -> Member -> Bool
--     isBestMember _  '[]     _ = True
--     isBestMember as (m:ms) n
--         | m /= n    = isBetterMember as n m && isBestMember as ms n
--         | otherwise = isBestMember as ms n -- skip members equal to 'n'
--
type family IsBestMember (as::[*]) (ms::[[*]]) (n :: [*]) :: Bool where
    IsBestMember as '[]       n = True
    IsBestMember as (m ': ms) n =
        If (m == n) (IsBestMember as ms n)
                      ((IsBetterMember as n m) && (IsBestMember as ms n))


-- | @'FilterApp' as ms@ returns the list of members from @ms@ that are
--   applicable with respect to the argument list @as@.
type family    FilterApp  (as::[*])  (ms::[[*]]) :: [[*]] where
    FilterApp  as  '[]        = '[]
    FilterApp  as  (m ': ms)  = FilterApp' as m (FilterApp as ms)
-- was: If (IsApp as m) (m ': FilterApp as ms)
--                      (FilterApp as ms)

-- Note: use of FilterApp' increases compilation speed by a factor of 10 (!)
type family    FilterApp' (as::[*]) (m :: [*]) (fas::[[*]]) :: [[*]] where
    FilterApp' as m fas = If (IsApp as m) (m ': fas) fas


-- | @'IsApp' as ps@ is true iff the function member defined by the 
--   parameters @ps@ is applicable with respect to the argument list @as@
--   (ref 7.4.2.1).  This means they of the same length and an implicit
--   conversion exists from the argument type to the corresponding
--   parameter on the function member.
type family    IsApp  as          ps :: Bool where
    IsApp  '[]        '[]       = True
    IsApp  '[]        (p ': ps) = False -- different lengths
    IsApp  (a ': as)  '[]       = False -- different lengths
    IsApp  (a ': as)  (p ': ps) = (ConvertsTo a p) && (IsApp as ps)


-- | @'IsBetterMember' as ps qs@ returns true iff the function member defined by the
--   parameters @ps@ is better than that given by parameters @qs@ given the list of
--   argument types @as@ (ref 7.4.2.2).
--
--     isBetterMember :: [Type] -> Member -> Member -> Bool
--     isBetterMember as ps qs = someBetter as ps qs && not (someBetter as qs ps)
--
type family IsBetterMember as ps qs :: Bool where
    IsBetterMember as ps qs = (AnyBetterConv as ps qs) && (Not (AnyBetterConv as qs ps))


type family    AnyBetterConv  as          ss          ts :: Bool where
    AnyBetterConv  '[]        '[]        '[]        = False
    AnyBetterConv  (a ': as)  (s ': ss)  (t ': ts)  = (IsBetterConv a s t) || (AnyBetterConv as ss ts)


type family    IsBetterConv t t1 t2 :: Bool where
    IsBetterConv t t1 t2 =    ( (t == t1)          && (Not (t == t2))          )
                           || ( (ConvertsTo t1 t2) && (Not (ConvertsTo t2 t1)) )

--
-- Original:
--
-- type instance  ConvertsTo t1 t2 =
--     ((||)  ((==) t1 t2)
--     ((||)  ((&&) (IsPrim t1)           ((==) t2 (Obj Object_)))
--     ((||)  ((&&) ((==) t1 Int32)       ((==) t2 Double))
--     ((||)  ((&&) ((==) t1 (Obj Null))  (IsRef t2))
--           ((&&) ((&&) (IsRef t1) (IsRef t2))
--                 (IsSubtypeOf t1 t2))))))
--

-- | @'ConvertsTo' s t@ returns true iff there is an implicit conversion from @s@
--   to @t@ (ref 6.1).
type family    ConvertsTo t1 t2 :: Bool where
    ConvertsTo t1 t2 =   (t1 == t2)
                    || ( (IsPrim t1)          &&  (t2 == (Obj Object_))  )
                    || ( (t1 == Int32)        &&  (t2 == Double)         )
                    || ( (t1 == (Obj Null) )  &&  (IsRef t2)             )
                    || ( (IsRef t1) && (IsRef t2) && (IsSubtypeOf t1 t2) )


--    ((||)5 ((==) t1 t2)
--          ((&&) (IsPrim t1)           ((==) t2 (Obj Object_)))
--          ((&&) ((==) t1 Int32)       ((==) t2 Double))
--          ((&&) ((==) t1 (Obj Null))  (IsRef t2))
--          ((&&)3 (IsRef t1) (IsRef t2) (IsSubtypeOf t1 t2)))
-- Note: using (||)5 instead of a nested set of (||)'s only gives a small performance improvement.


type family    IsSubtypeOf t1 t2 :: Bool where
    IsSubtypeOf t1 t2  = (t1 == t2) || (TyElem t2 (SupertypesOf t1))

type family    IsArr t :: Bool where
    IsArr t = (Not (t == (Obj Array_))) && (IsSubtypeOf t (Obj Array_))

{-
type family   ArrElemTy t
type instance ArrElemTy t =
    ArrElemTy' (TyCode t)
-}

{-
type family   ArrElemTy' tc
type instance ArrElemTy' (D0 ': DF ': tc) = tc
type family    IsSubtypeOf' t1 t2
type instance  IsSubtypeOf' t1 '[] = False -- we need this so that we can short-circuit evaluation
type instance  IsSubtypeOf' t1 (t2 ': t2s) = 
    (||) ((==)' t1 (t2 ': t2s))
        (TyElem' (t2 ': t2s) (SupertypesOf (FromTyCode t1)))

type family   (==)' t1 t2
type instance (==)' t1 t2 = DigitsEq t1 t2

type family    TyElem' t1 ts
type instance  TyElem' t1 '[]        = False
type instance  TyElem' t1 (t ': ts)  = (||) ((==)' t1 (TyCode t)) (TyElem' t1 ts) -- UGLY!
-}

-- | @'SupertypesOf' t@ is the list of supertypes of @t@.
type family SupertypesOf t :: [*]

type instance SupertypesOf String          = '[]
type instance SupertypesOf Int32           = '[]
type instance SupertypesOf Bool            = '[]
type instance SupertypesOf Double          = '[]
type instance SupertypesOf (Obj Null)      = '[]

-- vim:set sw=4 ts=4 expandtab:
