module Main where

import Encodings
import RegisterMachine
import ExampleRM
import IOTrace
import Gadgets
import UniversalRegisterMachine



main :: IO ()
main = printTrace addRMGadget [2,7]
