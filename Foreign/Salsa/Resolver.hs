{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, EmptyDataDecls, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Foreign.Salsa.Common
import Foreign.Salsa.TypePrelude

--
-- Basic predicates
--

type family    IsPrim t
type instance  IsPrim Int32      = TTrue
type instance  IsPrim String     = TTrue
type instance  IsPrim Bool       = TTrue
type instance  IsPrim Double     = TTrue
type instance  IsPrim (Obj t)    = TFalse

type family    IsRef t
type instance  IsRef t = TNot (IsPrim t)
-- FIXME: This definition of IsRef is incorrect.  String is a reference
--        type (in .NET) but it is also a primitive bridge type.

--
-- Operations on the encoded (Gödel) representations of .NET types:
--

-- | 'TyCode' maps labels representing .NET types to a type-level list of hexadecimal
--   values (similar to Gödel numbering) which can (then) be compared for equality.
--   This avoids having to define a large type function for comparing every pair
--   of labels used to represent .NET types.
type family TyCode t

type instance TyCode Int32              = D0 ::: TNil
type instance TyCode String             = D1 ::: TNil
type instance TyCode Bool               = D2 ::: TNil
type instance TyCode Double             = D3 ::: TNil
type instance TyCode (Obj Null)         = D4 ::: TNil
type instance TyCode (Maybe Bool)       = D5 ::: TNil -- Temporary HACK


type family FromTyCode t
type instance FromTyCode (D0 ::: TNil) = Int32
type instance FromTyCode (D1 ::: TNil) = String
type instance FromTyCode (D2 ::: TNil) = Bool
type instance FromTyCode (D3 ::: TNil) = Double
type instance FromTyCode (D4 ::: TNil) = Obj Null
type instance FromTyCode (D5 ::: TNil) = Maybe Bool


-- | 'TyEq t1 t2' is true iff the types @t1@ and @t2@ are the same .NET type
--   (as indicated by their code).
type family TyEq t1 t2
type instance TyEq t1 t2 = DigitsEq (TyCode t1) (TyCode t2)


-- | 'MemberEq m n' is true iff the members 'm' and 'n' have the same types.
type family MemberEq m n
type instance MemberEq TNil       TNil       = TTrue
type instance MemberEq TNil       (n ::: ns) = TFalse
type instance MemberEq (m ::: ms) TNil       = TFalse
type instance MemberEq (m ::: ms) (n ::: ns) = TAnd (TyEq m n) (MemberEq ms ns)

-- | @'TyElem' t ts@ is true iff the type @t@ is present in the list @ts@
--   (containing coded type representations).
type family    TyElem t1 ts
type instance  TyElem t1 TNil        = TFalse
type instance  TyElem t1 (t ::: ts)  = TOr (TyEq t1 t) (TyElem t1 ts)

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
type family ResolveMember as ms
type instance ResolveMember as ms = ResolveMember' as (FilterApp as ms)
-- was: FromSingleton (Error NoMatch) (FilterBestMembers as (FilterApp as ms) (FilterApp as ms))

type family ResolveMember' as fa
type instance ResolveMember' as fa = FromSingleton (Error NoMatch) (FilterBestMembers as fa fa)


data Error x
data NoMatch


-- | @'FilterBestMembers' as ms ms@ returns the list of members from @ms@ that are
--   better than all the other members in @ms@ (with respect to the argument list
--   @as@). 
--
--     filterBestMembers :: [Type] -> [Member] -> [Member] -> [Member]
--     filterBestMembers as ms ns = filter (isBestMember as ms) ns
--
type family FilterBestMembers as ms ns
type instance FilterBestMembers as ms TNil       = TNil
type instance FilterBestMembers as ms (n ::: ns) = FilterBestMembers' as ms n (FilterBestMembers as ms ns)
-- was: If (IsBestMember as ms n) (n ::: (FilterBestMembers as ms ns))
--                                (FilterBestMembers as ms ns)

type family FilterBestMembers' as ms n fbms
type instance FilterBestMembers' as ms n fbms = If (IsBestMember as ms n) (n ::: fbms) fbms


-- | @'IsBestMember' as ms n@ is true iff the member @n@ is better than all
--   the (other) members in @ms@ with respect to the argument list @as@. 
--
--     isBestMember :: [Type] -> [Member] -> Member -> Bool
--     isBestMember _  []     _ = True
--     isBestMember as (m:ms) n
--         | m /= n    = isBetterMember as n m && isBestMember as ms n
--         | otherwise = isBestMember as ms n -- skip members equal to 'n'
--
type family IsBestMember as ms n
type instance IsBestMember as TNil       n = TTrue
type instance IsBestMember as (m ::: ms) n =
    If (MemberEq m n) (IsBestMember as ms n)
                      (TAnd (IsBetterMember as n m) (IsBestMember as ms n))


-- | @'FilterApp' as ms@ returns the list of members from @ms@ that are
--   applicable with respect to the argument list @as@.
type family    FilterApp  as  ms
type instance  FilterApp  as  TNil        = TNil
type instance  FilterApp  as  (m ::: ms)  = FilterApp' as m (FilterApp as ms)
-- was: If (IsApp as m) (m ::: FilterApp as ms)
--                      (FilterApp as ms)

-- Note: use of FilterApp' increases compilation speed by a factor of 10 (!)
type family    FilterApp' as m fas
type instance  FilterApp' as m fas = If (IsApp as m) (m ::: fas) fas


-- | @'IsApp' as ps@ is true iff the function member defined by the 
--   parameters @ps@ is applicable with respect to the argument list @as@
--   (ref 7.4.2.1).  This means they of the same length and an implicit
--   conversion exists from the argument type to the corresponding
--   parameter on the function member.
type family    IsApp  as          ps
type instance  IsApp  TNil        TNil       = TTrue
type instance  IsApp  TNil        (p ::: ps) = TFalse -- different lengths
type instance  IsApp  (a ::: as)  TNil       = TFalse -- different lengths
type instance  IsApp  (a ::: as)  (p ::: ps) = TAnd (ConvertsTo a p) (IsApp as ps)


-- | @'IsBetterMember' as ps qs@ returns true iff the function member defined by the
--   parameters @ps@ is better than that given by parameters @qs@ given the list of
--   argument types @as@ (ref 7.4.2.2).
--
--     isBetterMember :: [Type] -> Member -> Member -> Bool
--     isBetterMember as ps qs = someBetter as ps qs && not (someBetter as qs ps)
--
type family IsBetterMember as ps qs
type instance IsBetterMember as ps qs =
    TAnd (AnyBetterConv as ps qs) 
         (TNot (AnyBetterConv as qs ps))


type family    AnyBetterConv  as          ss          ts
type instance  AnyBetterConv  TNil        TNil        TNil        = TFalse
type instance  AnyBetterConv  (a ::: as)  (s ::: ss)  (t ::: ts)  =
    TOr (IsBetterConv a s t) (AnyBetterConv as ss ts)


type family    IsBetterConv t t1 t2
type instance  IsBetterConv t t1 t2 =
    TOr  (TAnd (TyEq t t1) (TNot (TyEq t t2)))
         (TAnd (ConvertsTo t1 t2) (TNot (ConvertsTo t2 t1)))

--
-- Original:
--
-- type instance  ConvertsTo t1 t2 =
--     (TOr  (TyEq t1 t2)
--     (TOr  (TAnd (IsPrim t1)           (TyEq t2 (Obj Object_)))
--     (TOr  (TAnd (TyEq t1 Int32)       (TyEq t2 Double))
--     (TOr  (TAnd (TyEq t1 (Obj Null))  (IsRef t2))
--           (TAnd (TAnd (IsRef t1) (IsRef t2))
--                 (IsSubtypeOf t1 t2))))))
--

-- | @'ConvertsTo' s t@ returns true iff there is an implicit conversion from @s@
--   to @t@ (ref 6.1).
type family    ConvertsTo t1 t2
type instance  ConvertsTo t1 t2 =
    (TOr  (TyEq t1 t2)
    (TOr  (TAnd (IsPrim t1)           (TyEq t2 (Obj Object_)))
    (TOr  (TAnd (TyEq t1 Int32)       (TyEq t2 Double))
    (TOr  (TAnd (TyEq t1 (Obj Null))  (IsRef t2))
--    (TOr  (TAnd (TAnd (IsArr t1) (IsArr t2))
--                (IsSubtypeOf' (ArrElemTy t1) (ArrElemTy t2)))
          (TAnd (TAnd (IsRef t1) (IsRef t2))
                (IsSubtypeOf t1 t2)))))) -- )


--    (TOr5 (TyEq t1 t2)
--          (TAnd (IsPrim t1)           (TyEq t2 (Obj Object_)))
--          (TAnd (TyEq t1 Int32)       (TyEq t2 Double))
--          (TAnd (TyEq t1 (Obj Null))  (IsRef t2))
--          (TAnd3 (IsRef t1) (IsRef t2) (IsSubtypeOf t1 t2)))
-- Note: using TOr5 instead of a nested set of TOr's only gives a small performance improvement.


type family    IsSubtypeOf t1 t2
type instance  IsSubtypeOf t1 t2  = 
    TOr (TyEq t1 t2)
        (TyElem t2 (SupertypesOf t1))

type family    IsArr t
type instance  IsArr t = TAnd (TNot (TyEq t (Obj Array_)))
                              (IsSubtypeOf t (Obj Array_))

type family   ArrElemTy t
type instance ArrElemTy t =
    ArrElemTy' (TyCode t)

type family   ArrElemTy' tc
type instance ArrElemTy' (D0 ::: DF ::: tc) = tc
type family    IsSubtypeOf' t1 t2
type instance  IsSubtypeOf' t1 TNil = TFalse -- we need this so that we can short-circuit evaluation
type instance  IsSubtypeOf' t1 (t2 ::: t2s) = 
    TOr (TyEq' t1 (t2 ::: t2s))
        (TyElem' (t2 ::: t2s) (SupertypesOf (FromTyCode t1)))

type family   TyEq' t1 t2
type instance TyEq' t1 t2 = DigitsEq t1 t2

type family    TyElem' t1 ts
type instance  TyElem' t1 TNil        = TFalse
type instance  TyElem' t1 (t ::: ts)  = TOr (TyEq' t1 (TyCode t)) (TyElem' t1 ts) -- UGLY!

-- | @'SupertypesOf' t@ is the list of supertypes of @t@.
type family SupertypesOf t

type instance SupertypesOf String          = TNil
type instance SupertypesOf Int32           = TNil
type instance SupertypesOf Bool            = TNil
type instance SupertypesOf Double          = TNil
type instance SupertypesOf (Obj Null)      = TNil

-- vim:set sw=4 ts=4 expandtab:
