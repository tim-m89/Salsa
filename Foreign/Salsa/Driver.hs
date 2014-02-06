{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Licence     : BSD-style (see LICENSE)
-- 
-- Embeded driver assembly that helps in generating bindings.
--
-----------------------------------------------------------------------------


module Foreign.Salsa.Driver (driverData) where

import qualified Data.ByteString.Char8 as B
import Data.FileEmbed

{-# NOINLINE driverData #-}
driverData :: B.ByteString
driverData = $(embedFile "Driver/Salsa.dll") 

