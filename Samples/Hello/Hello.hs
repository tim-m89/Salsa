module Main where

--
-- Displays 'Hello .NET World!' using .NET's Console.WriteLine() method.
--

import Foreign.Salsa
import Bindings

main :: IO ()
main = withCLR $ do
    _Console # _writeLine ("Hello .NET World!")
    _Console # _writeLine (2 :: Int32)
    _Console # _writeLine (3.14 :: Double)

-- vim:set sw=4 ts=4 expandtab:
