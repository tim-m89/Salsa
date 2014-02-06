{-# LANGUAGE TypeFamilies, TypeOperators, EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}
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

data TTrue  = TTrue
data TFalse = TFalse

type family TOr x y
type instance TOr TTrue  x  = TTrue
type instance TOr TFalse x  = x

type family TAnd x y
type instance TAnd TFalse x  = TFalse
type instance TAnd TTrue  x  = x

type family TNot x
type instance TNot TTrue  = TFalse
type instance TNot TFalse = TTrue

type family If c a b
type instance If TTrue  a b = a
type instance If TFalse a b = b

--type family TAnd3 x y z
--type instance TAnd3 TFalse y      z = TFalse
--type instance TAnd3 TTrue  TFalse z = TFalse
--type instance TAnd3 TTrue  TTrue  z = z

--type family TOr5 a b c d e
--type instance TOr5 TTrue  b      c      d      e = TTrue
--type instance TOr5 TFalse TTrue  c      d      e = TTrue
--type instance TOr5 TFalse TFalse TTrue  d      e = TTrue
--type instance TOr5 TFalse TFalse TFalse TTrue  e = TTrue
--type instance TOr5 TFalse TFalse TFalse TFalse e = e

--
-- Type-level lists and associated operations:
--

data TNil
data x ::: xs -- = x ::: xs
infixr 5 :::

type family    BoolEq x       y
type instance  BoolEq TTrue   TTrue   = TTrue
type instance  BoolEq TFalse  TFalse  = TTrue
type instance  BoolEq TFalse  TTrue   = TFalse
type instance  BoolEq TTrue   TFalse  = TFalse

-- | 'ListEq xs ys' is true if the given type-level boolean lists contain 
--   the same boolean values, and false otherwise.
type family    ListEq xs          ys
type instance  ListEq TNil        TNil        = TTrue
type instance  ListEq (x ::: xs)  TNil        = TFalse
type instance  ListEq TNil        (x ::: xs)  = TFalse
type instance  ListEq (x ::: xs)  (y ::: ys)  = TAnd (BoolEq x y) (ListEq xs ys)

-- | @'FromSingleton' xs def@ returns the only element of the list @xs@ (if it is a
--   single element list), otherwise it returns the default value @def@.
type family FromSingleton def xs
type instance FromSingleton def TNil            = def
type instance FromSingleton def (x ::: TNil)    = x
type instance FromSingleton def (x ::: y ::: z) = def

-- 
-- Type-level base-16 digits and equality:
--

data D0 = D0
data D1 = D1
data D2 = D2
data D3 = D3
data D4 = D4
data D5 = D5
data D6 = D6
data D7 = D7
data D8 = D8
data D9 = D9
data DA = DA
data DB = DB
data DC = DC
data DD = DD
data DE = DE
data DF = DF

type family    DigitEq x  y
type instance  DigitEq D0 D0 = TTrue
type instance  DigitEq D1 D0 = TFalse
type instance  DigitEq D2 D0 = TFalse
type instance  DigitEq D3 D0 = TFalse
type instance  DigitEq D4 D0 = TFalse
type instance  DigitEq D5 D0 = TFalse
type instance  DigitEq D6 D0 = TFalse
type instance  DigitEq D7 D0 = TFalse
type instance  DigitEq D8 D0 = TFalse
type instance  DigitEq D9 D0 = TFalse
type instance  DigitEq DA D0 = TFalse
type instance  DigitEq DB D0 = TFalse
type instance  DigitEq DC D0 = TFalse
type instance  DigitEq DD D0 = TFalse
type instance  DigitEq DE D0 = TFalse
type instance  DigitEq DF D0 = TFalse
type instance  DigitEq D0 D1 = TFalse
type instance  DigitEq D1 D1 = TTrue
type instance  DigitEq D2 D1 = TFalse
type instance  DigitEq D3 D1 = TFalse
type instance  DigitEq D4 D1 = TFalse
type instance  DigitEq D5 D1 = TFalse
type instance  DigitEq D6 D1 = TFalse
type instance  DigitEq D7 D1 = TFalse
type instance  DigitEq D8 D1 = TFalse
type instance  DigitEq D9 D1 = TFalse
type instance  DigitEq DA D1 = TFalse
type instance  DigitEq DB D1 = TFalse
type instance  DigitEq DC D1 = TFalse
type instance  DigitEq DD D1 = TFalse
type instance  DigitEq DE D1 = TFalse
type instance  DigitEq DF D1 = TFalse
type instance  DigitEq D0 D2 = TFalse
type instance  DigitEq D1 D2 = TFalse
type instance  DigitEq D2 D2 = TTrue
type instance  DigitEq D3 D2 = TFalse
type instance  DigitEq D4 D2 = TFalse
type instance  DigitEq D5 D2 = TFalse
type instance  DigitEq D6 D2 = TFalse
type instance  DigitEq D7 D2 = TFalse
type instance  DigitEq D8 D2 = TFalse
type instance  DigitEq D9 D2 = TFalse
type instance  DigitEq DA D2 = TFalse
type instance  DigitEq DB D2 = TFalse
type instance  DigitEq DC D2 = TFalse
type instance  DigitEq DD D2 = TFalse
type instance  DigitEq DE D2 = TFalse
type instance  DigitEq DF D2 = TFalse
type instance  DigitEq D0 D3 = TFalse
type instance  DigitEq D1 D3 = TFalse
type instance  DigitEq D2 D3 = TFalse
type instance  DigitEq D3 D3 = TTrue
type instance  DigitEq D4 D3 = TFalse
type instance  DigitEq D5 D3 = TFalse
type instance  DigitEq D6 D3 = TFalse
type instance  DigitEq D7 D3 = TFalse
type instance  DigitEq D8 D3 = TFalse
type instance  DigitEq D9 D3 = TFalse
type instance  DigitEq DA D3 = TFalse
type instance  DigitEq DB D3 = TFalse
type instance  DigitEq DC D3 = TFalse
type instance  DigitEq DD D3 = TFalse
type instance  DigitEq DE D3 = TFalse
type instance  DigitEq DF D3 = TFalse
type instance  DigitEq D0 D4 = TFalse
type instance  DigitEq D1 D4 = TFalse
type instance  DigitEq D2 D4 = TFalse
type instance  DigitEq D3 D4 = TFalse
type instance  DigitEq D4 D4 = TTrue
type instance  DigitEq D5 D4 = TFalse
type instance  DigitEq D6 D4 = TFalse
type instance  DigitEq D7 D4 = TFalse
type instance  DigitEq D8 D4 = TFalse
type instance  DigitEq D9 D4 = TFalse
type instance  DigitEq DA D4 = TFalse
type instance  DigitEq DB D4 = TFalse
type instance  DigitEq DC D4 = TFalse
type instance  DigitEq DD D4 = TFalse
type instance  DigitEq DE D4 = TFalse
type instance  DigitEq DF D4 = TFalse
type instance  DigitEq D0 D5 = TFalse
type instance  DigitEq D1 D5 = TFalse
type instance  DigitEq D2 D5 = TFalse
type instance  DigitEq D3 D5 = TFalse
type instance  DigitEq D4 D5 = TFalse
type instance  DigitEq D5 D5 = TTrue
type instance  DigitEq D6 D5 = TFalse
type instance  DigitEq D7 D5 = TFalse
type instance  DigitEq D8 D5 = TFalse
type instance  DigitEq D9 D5 = TFalse
type instance  DigitEq DA D5 = TFalse
type instance  DigitEq DB D5 = TFalse
type instance  DigitEq DC D5 = TFalse
type instance  DigitEq DD D5 = TFalse
type instance  DigitEq DE D5 = TFalse
type instance  DigitEq DF D5 = TFalse
type instance  DigitEq D0 D6 = TFalse
type instance  DigitEq D1 D6 = TFalse
type instance  DigitEq D2 D6 = TFalse
type instance  DigitEq D3 D6 = TFalse
type instance  DigitEq D4 D6 = TFalse
type instance  DigitEq D5 D6 = TFalse
type instance  DigitEq D6 D6 = TTrue
type instance  DigitEq D7 D6 = TFalse
type instance  DigitEq D8 D6 = TFalse
type instance  DigitEq D9 D6 = TFalse
type instance  DigitEq DA D6 = TFalse
type instance  DigitEq DB D6 = TFalse
type instance  DigitEq DC D6 = TFalse
type instance  DigitEq DD D6 = TFalse
type instance  DigitEq DE D6 = TFalse
type instance  DigitEq DF D6 = TFalse
type instance  DigitEq D0 D7 = TFalse
type instance  DigitEq D1 D7 = TFalse
type instance  DigitEq D2 D7 = TFalse
type instance  DigitEq D3 D7 = TFalse
type instance  DigitEq D4 D7 = TFalse
type instance  DigitEq D5 D7 = TFalse
type instance  DigitEq D6 D7 = TFalse
type instance  DigitEq D7 D7 = TTrue
type instance  DigitEq D8 D7 = TFalse
type instance  DigitEq D9 D7 = TFalse
type instance  DigitEq DA D7 = TFalse
type instance  DigitEq DB D7 = TFalse
type instance  DigitEq DC D7 = TFalse
type instance  DigitEq DD D7 = TFalse
type instance  DigitEq DE D7 = TFalse
type instance  DigitEq DF D7 = TFalse
type instance  DigitEq D0 D8 = TFalse
type instance  DigitEq D1 D8 = TFalse
type instance  DigitEq D2 D8 = TFalse
type instance  DigitEq D3 D8 = TFalse
type instance  DigitEq D4 D8 = TFalse
type instance  DigitEq D5 D8 = TFalse
type instance  DigitEq D6 D8 = TFalse
type instance  DigitEq D7 D8 = TFalse
type instance  DigitEq D8 D8 = TTrue
type instance  DigitEq D9 D8 = TFalse
type instance  DigitEq DA D8 = TFalse
type instance  DigitEq DB D8 = TFalse
type instance  DigitEq DC D8 = TFalse
type instance  DigitEq DD D8 = TFalse
type instance  DigitEq DE D8 = TFalse
type instance  DigitEq DF D8 = TFalse
type instance  DigitEq D0 D9 = TFalse
type instance  DigitEq D1 D9 = TFalse
type instance  DigitEq D2 D9 = TFalse
type instance  DigitEq D3 D9 = TFalse
type instance  DigitEq D4 D9 = TFalse
type instance  DigitEq D5 D9 = TFalse
type instance  DigitEq D6 D9 = TFalse
type instance  DigitEq D7 D9 = TFalse
type instance  DigitEq D8 D9 = TFalse
type instance  DigitEq D9 D9 = TTrue
type instance  DigitEq DA D9 = TFalse
type instance  DigitEq DB D9 = TFalse
type instance  DigitEq DC D9 = TFalse
type instance  DigitEq DD D9 = TFalse
type instance  DigitEq DE D9 = TFalse
type instance  DigitEq DF D9 = TFalse
type instance  DigitEq D0 DA = TFalse
type instance  DigitEq D1 DA = TFalse
type instance  DigitEq D2 DA = TFalse
type instance  DigitEq D3 DA = TFalse
type instance  DigitEq D4 DA = TFalse
type instance  DigitEq D5 DA = TFalse
type instance  DigitEq D6 DA = TFalse
type instance  DigitEq D7 DA = TFalse
type instance  DigitEq D8 DA = TFalse
type instance  DigitEq D9 DA = TFalse
type instance  DigitEq DA DA = TTrue
type instance  DigitEq DB DA = TFalse
type instance  DigitEq DC DA = TFalse
type instance  DigitEq DD DA = TFalse
type instance  DigitEq DE DA = TFalse
type instance  DigitEq DF DA = TFalse
type instance  DigitEq D0 DB = TFalse
type instance  DigitEq D1 DB = TFalse
type instance  DigitEq D2 DB = TFalse
type instance  DigitEq D3 DB = TFalse
type instance  DigitEq D4 DB = TFalse
type instance  DigitEq D5 DB = TFalse
type instance  DigitEq D6 DB = TFalse
type instance  DigitEq D7 DB = TFalse
type instance  DigitEq D8 DB = TFalse
type instance  DigitEq D9 DB = TFalse
type instance  DigitEq DA DB = TFalse
type instance  DigitEq DB DB = TTrue
type instance  DigitEq DC DB = TFalse
type instance  DigitEq DD DB = TFalse
type instance  DigitEq DE DB = TFalse
type instance  DigitEq DF DB = TFalse
type instance  DigitEq D0 DC = TFalse
type instance  DigitEq D1 DC = TFalse
type instance  DigitEq D2 DC = TFalse
type instance  DigitEq D3 DC = TFalse
type instance  DigitEq D4 DC = TFalse
type instance  DigitEq D5 DC = TFalse
type instance  DigitEq D6 DC = TFalse
type instance  DigitEq D7 DC = TFalse
type instance  DigitEq D8 DC = TFalse
type instance  DigitEq D9 DC = TFalse
type instance  DigitEq DA DC = TFalse
type instance  DigitEq DB DC = TFalse
type instance  DigitEq DC DC = TTrue
type instance  DigitEq DD DC = TFalse
type instance  DigitEq DE DC = TFalse
type instance  DigitEq DF DC = TFalse
type instance  DigitEq D0 DD = TFalse
type instance  DigitEq D1 DD = TFalse
type instance  DigitEq D2 DD = TFalse
type instance  DigitEq D3 DD = TFalse
type instance  DigitEq D4 DD = TFalse
type instance  DigitEq D5 DD = TFalse
type instance  DigitEq D6 DD = TFalse
type instance  DigitEq D7 DD = TFalse
type instance  DigitEq D8 DD = TFalse
type instance  DigitEq D9 DD = TFalse
type instance  DigitEq DA DD = TFalse
type instance  DigitEq DB DD = TFalse
type instance  DigitEq DC DD = TFalse
type instance  DigitEq DD DD = TTrue
type instance  DigitEq DE DD = TFalse
type instance  DigitEq DF DD = TFalse
type instance  DigitEq D0 DE = TFalse
type instance  DigitEq D1 DE = TFalse
type instance  DigitEq D2 DE = TFalse
type instance  DigitEq D3 DE = TFalse
type instance  DigitEq D4 DE = TFalse
type instance  DigitEq D5 DE = TFalse
type instance  DigitEq D6 DE = TFalse
type instance  DigitEq D7 DE = TFalse
type instance  DigitEq D8 DE = TFalse
type instance  DigitEq D9 DE = TFalse
type instance  DigitEq DA DE = TFalse
type instance  DigitEq DB DE = TFalse
type instance  DigitEq DC DE = TFalse
type instance  DigitEq DD DE = TFalse
type instance  DigitEq DE DE = TTrue
type instance  DigitEq DF DE = TFalse
type instance  DigitEq D0 DF = TFalse
type instance  DigitEq D1 DF = TFalse
type instance  DigitEq D2 DF = TFalse
type instance  DigitEq D3 DF = TFalse
type instance  DigitEq D4 DF = TFalse
type instance  DigitEq D5 DF = TFalse
type instance  DigitEq D6 DF = TFalse
type instance  DigitEq D7 DF = TFalse
type instance  DigitEq D8 DF = TFalse
type instance  DigitEq D9 DF = TFalse
type instance  DigitEq DA DF = TFalse
type instance  DigitEq DB DF = TFalse
type instance  DigitEq DC DF = TFalse
type instance  DigitEq DD DF = TFalse
type instance  DigitEq DE DF = TFalse
type instance  DigitEq DF DF = TTrue

type family    DigitsEq xs          ys
type instance  DigitsEq TNil        TNil        = TTrue
type instance  DigitsEq (x ::: xs)  TNil        = TFalse
type instance  DigitsEq TNil        (x ::: xs)  = TFalse
type instance  DigitsEq (x ::: xs)  (y ::: ys)  = TAnd (DigitEq x y) (DigitsEq xs ys)

-- vim:set sw=4 ts=4 expandtab:
