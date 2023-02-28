module ExampleRM where

import Encodings
import RegisterMachine
import Gadgets (addGadget, accGadget, lenAccGadget)


addRM :: [Instruction]
addRM = [
    Dec 1 1 2,
    Inc 0 0,
    Dec 2 3 4,
    Inc 0 2,
    Halt
    ]


addRMGadget :: [Instruction]
addRMGadget = 
    accGadget 1 0 0 ++
    accGadget 2 0 lenAccGadget ++
    [Halt]


multRM :: [Instruction]
multRM = [
    Dec 1 1 6,
    Dec 2 2 4,
    Inc 0 3,
    Inc 3 1,
    Dec 3 5 0,
    Inc 2 4,
    Halt
    ]