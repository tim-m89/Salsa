{-# LANGUAGE TypeFamilies, TypeOperators, EmptyDataDecls, UndecidableInstances, DataKinds, PolyKinds, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Type-level implementations of some standard boolean and list functions.
-- Including short-circuited 'and' and 'or' logic functions.
--
-----------------------------------------------------------------------------
module Foreign.Salsa.TypePrelude where

--
-- Type-level booleans and boolean operations:
--

data TBool where
    TTrue :: TBool
    TFalse :: TBool


type family TOr x y where
    TOr TTrue  x  = TTrue
    TOr TFalse x  = x

type family TAnd x y where
    TAnd TFalse x  = TFalse
    TAnd TTrue  x  = x

type family TNot x where
    TNot TTrue  = TFalse
    TNot TFalse = TTrue

type family If c a b where
    If TTrue  a b = a
    If TFalse a b = b

--
-- Type-level lists and associated operations:
--

data TNil
data x ::: xs -- = x ::: xs
infixr 5 :::

type family BoolEq x y where
    BoolEq TTrue   TTrue   = TTrue
    BoolEq TFalse  TFalse  = TTrue
    BoolEq TFalse  TTrue   = TFalse
    BoolEq TTrue   TFalse  = TFalse

type family ListEqq x y xs ys where
    ListEqq a b c d = BoolEq a b
    
-- | 'ListEq xs ys' is true if the given type-level boolean lists contain 
--   the same boolean values, and false otherwise.
type family    ListEq xs          ys where
    ListEq TNil        TNil        = TTrue
    ListEq (x ::: xs)  TNil        = TFalse
    ListEq TNil        (x ::: xs)  = TFalse
    ListEq (x ::: xs)  (y ::: ys)  = ListEqq x y xs ys

-- | @'FromSingleton' xs def@ returns the only element of the list @xs@ (if it is a
--   single element list), otherwise it returns the default value @def@.
type family FromSingleton def xs where
    FromSingleton def (x ::: TNil)    = x
    FromSingleton def x               = def

-- vim:set sw=4 ts=4 expandtab:
