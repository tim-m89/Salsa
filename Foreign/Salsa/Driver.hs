{-# LANGUAGE TemplateHaskell #-}

module Foreign.Salsa.Driver (driverData) where

import qualified Data.ByteString.Char8 as B
import Data.FileEmbed

{-# NOINLINE driverData #-}
driverData :: B.ByteString
driverData = $(embedFile "Driver/Salsa.dll") 

