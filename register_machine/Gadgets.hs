module Gadgets where

import Encodings
import RegisterMachine

{-
All gadgets (except popGadget where it is the penultimate parameter)
take as their last parameter a label number. This is the label of
the first instruction in the gadget, and acts as an offset, so that
this gadget can be inserted into other register machines with the
labels lining up.

When using a gadget, the value for l should be the enrty point
ie the label where the gadget starts
eg [Inc 0 1, Inc 1 2] ++ zeroGadget 0 2 ++ [Dec 1 4 0, Halt]

Gadgets themselves should never contain a Halt instruction.
A gadget should exit by jumping to the label 1 after the last
instruction of the gadget.
  Li-1: ___
+---Gadget g---
| Li:   ___
| Li+1: ___
| ...
| ln-1: ___
+--------------
  Ln:   ___

This gadget g should exit by jumping to label Ln.
If a gadget is intended to cause a Halt then the instruction
Ln should be a Halt instruction, outside the definition of g.
Alternatively, not defining Ln will cause gadget g to
eroneously halt on exit.
-}

zeroGadget :: Register -> Integer -> [Instruction]
-- set register r to 0, starting at instruction l
zeroGadget r l = [
    Dec r l (l+1)            --L: l
    ]                        --L: l+1

moveGadget :: Register -> Register -> Integer -> [Instruction]
-- move the contents of register ri to register rj, leaving ri zeroed, starting at instruction l
moveGadget ri rj l = 
    zeroGadget rj l ++ [     --L: l
    Dec ri (l+2) (l+3),      --L: l+1
    Inc rj (l+1)             --L: l+2
    ]                        --L: l+3

accGadget :: Register -> Register -> Integer -> [Instruction]
-- add register ri to register rj, leaving ri zeroed, starting at instruction l
accGadget ri rj l = [
    Dec ri (l+1) (l+2),      --L: l
    Inc rj l                 --L: l+1
    ]

addGadget :: Register -> Register -> Register -> Integer -> [Instruction]
-- add register ri to register rj, leaving ri intact, using scratch register s, starting at instruction l
addGadget ri rj s l = 
    zeroGadget s l ++ [      --L: l
    Dec ri (l+2) (l+4),      --L: l+1
    Inc rj (l+3),            --L: l+2
    Inc s (l+1)] ++          --L: l+3
    moveGadget s ri (l+4)    --L: l+4 -> l+6
                             --L: l+7

copyGadget :: Register -> Register -> Register -> Integer -> [Instruction]
-- copy the contents of register ri to register rj, using register s as a scratch register, starting at instruction l
copyGadget ri rj s l = 
    zeroGadget rj l ++       --L: l
    addGadget ri rj s (l+1)  --L: l+1 -> l+7
                             --L: l+8

doubleGadget :: Register -> Register -> Integer -> [Instruction]
-- double the contents of register r using register s as a scratch register, starting at instruction l
doubleGadget r s l = [
    Dec r (l+1) (l+3),       --L: l
    Inc s (l+2),             --L: l+1
    Inc s l] ++              --L: l+2
    moveGadget s r (l+3)     --L: l+3 -> l+5
                             --L: l+6

pushGadget :: Register -> Register -> Register -> Integer -> [Instruction]
-- push the value in register ri onto the list in register rj, leaving ri zeroed, using scratch register s, starting at instruction l
pushGadget ri rj s l = 
    zeroGadget s l ++ [         --L: l
    Inc s (l+2),                --L: l+1
    Dec rj (l+3) (l+4),         --L: l+2
    Inc s (l+1)] ++             --L: l+3
    moveGadget s rj (l+4) ++ [  --L: l+4 -> l+6
    Dec ri (l+2) (l+8)          --L: l+7
    ]                           --L: l+8

popGadget :: Register -> Register -> Register -> Integer -> Integer -> [Instruction]
-- pop the head of the list in register ri into register rj, using scratch register s,
--  starting at instruction l, and jumping to l+9 on success and f on failure
popGadget ri rj s l f = 
    zeroGadget rj l ++ [        --L: l
    Dec ri (l+2) f,             --L: l+1
    Inc ri (l+3)] ++            --L: l+2
    moveGadget ri s (l+3) ++ [  --L: l+3 -> l+5
    Dec s (l+7) (l+9),          --L: l+6
    Dec s (l+8) (l+10),         --L: l+7
    Inc ri (l+6),               --L: l+8
    Inc rj (l+3)                --L: l+9
    ]                           --L: l+10


lenZeroGadget :: Integer
lenZeroGadget = 1 --fromIntegral $ length (zeroGadget 0 0)

lenMoveGadget :: Integer
lenMoveGadget = 3 --fromIntegral $ length (moveGadget 0 0 0)

lenAccGadget :: Integer
lenAccGadget = 2 --fromIntegral $ length (accGadget 0 0 0)

lenAddGadget :: Integer
lenAddGadget = 7 --fromIntegral $ length (addGadget 0 0 0 0)

lenCopyGadget :: Integer
lenCopyGadget = 8 --fromIntegral $ length (copyGadget 0 0 0 0)

lenDoubleGadget :: Integer
lenDoubleGadget = 6 --fromIntegral $ length (doubleGadget 0 0 0)

lenPushGadget :: Integer
lenPushGadget = 8 --fromIntegral $ length (pushGadget 0 0 0 0)

lenPopGadget :: Integer
lenPopGadget = 10 --fromIntegral $ length (popGadget 0 0 0 0 0)

--lengths :: [Integer]
--lengths = [lenZeroGadget, lenMoveGadget, lenAccGadget, lenAddGadget, lenCopyGadget, lenDoubleGadget, lenPushGadget, lenPopGadget]