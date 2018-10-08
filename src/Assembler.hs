{-# LANGUAGE BinaryLiterals #-}

module Assembler (assemble) where

import           Data.Char   (intToDigit)
import           Lexer       (Token (..))
import           Parser      (Computation (..), Destination, Instruction (..),
                              Jump (..), Register (..))
import           Text.Printf (printf)

assemble :: Instruction -> String
assemble i = case i of
              a@(AInstruction _) -> assembleAInstruction a
              c@CInstruction{}   -> assembleCInstruction c

assembleAInstruction :: Instruction -> String
assembleAInstruction (AInstruction (Number n)) = '0' : intToBin n 15

assembleCInstruction :: Instruction -> String
assembleCInstruction c@CInstruction{} =
  let destNum = getDestNum $ destination c
      compNum = getCompNum $ computation c
      jumpNum = getJumpNum $ jump c
  in "111" <> intToBin compNum 7 <> intToBin destNum 3 <> intToBin jumpNum 3

getDestNum :: [Destination] -> Int
getDestNum []     = 0
getDestNum (M:ds) = 1 + getDestNum ds
getDestNum (D:ds) = 2 + getDestNum ds
getDestNum (A:ds) = 4 + getDestNum ds

getCompNum :: Computation -> Int
getCompNum (ConstantOperation (Number 0))                   = 0b101010
getCompNum (ConstantOperation (Number 1))                   = 0b111111
getCompNum (ConstantOperation (Identifier "D"))             = 0b001100
getCompNum (ConstantOperation (Identifier "A"))             = 0b110000
getCompNum (ConstantOperation (Identifier "M"))             = 0b1110000
getCompNum (UnaryOperation (Operator '!') (Identifier "D")) = 0b001101
getCompNum (UnaryOperation (Operator '!') (Identifier "A")) = 0b110001
getCompNum (UnaryOperation (Operator '!') (Identifier "M")) = 0b1110001
getCompNum (UnaryOperation (Operator '-') (Number 1))       = 0b111010
getCompNum (UnaryOperation (Operator '-') (Identifier "D")) = 0b001111
getCompNum (UnaryOperation (Operator '-') (Identifier "A")) = 0b110011
getCompNum (UnaryOperation (Operator '-') (Identifier "M")) = 0b1110011
getCompNum (BinaryOperation (Identifier "D") (Operator '+') (Number 1)) = 0b011111
getCompNum (BinaryOperation (Identifier "A") (Operator '+') (Number 1)) = 0b110111
getCompNum (BinaryOperation (Identifier "M") (Operator '+') (Number 1)) = 0b1110111
getCompNum (BinaryOperation (Identifier "D") (Operator '-') (Number 1)) = 0b001110
getCompNum (BinaryOperation (Identifier "A") (Operator '-') (Number 1)) = 0b110010
getCompNum (BinaryOperation (Identifier "M") (Operator '-') (Number 1)) = 0b1110010
getCompNum (BinaryOperation (Identifier "D") (Operator '+') (Identifier "A")) = 0b000010
getCompNum (BinaryOperation (Identifier "D") (Operator '+') (Identifier "M")) = 0b1000010
getCompNum (BinaryOperation (Identifier "D") (Operator '-') (Identifier "A")) = 0b010011
getCompNum (BinaryOperation (Identifier "D") (Operator '-') (Identifier "M")) = 0b1010011
getCompNum (BinaryOperation (Identifier "A") (Operator '-') (Identifier "D")) = 0b000111
getCompNum (BinaryOperation (Identifier "M") (Operator '-') (Identifier "D")) = 0b1000111
getCompNum (BinaryOperation (Identifier "D") (Operator '&') (Identifier "A")) = 0b000000
getCompNum (BinaryOperation (Identifier "D") (Operator '&') (Identifier "M")) = 0b1000000
getCompNum (BinaryOperation (Identifier "D") (Operator '|') (Identifier "A")) = 0b010101
getCompNum (BinaryOperation (Identifier "D") (Operator '|') (Identifier "M")) = 0b1010101

getJumpNum :: Maybe Jump -> Int
getJumpNum (Just JGT) = 0b001
getJumpNum (Just JEQ) = 0b010
getJumpNum (Just JGE) = 0b011
getJumpNum (Just JLT) = 0b100
getJumpNum (Just JNE) = 0b101
getJumpNum (Just JLE) = 0b110
getJumpNum (Just JMP) = 0b111
getJumpNum Nothing    = 0b000

intToBin :: Int -> Int -> String
intToBin i pad = let bin = printf ("%-0." <> show pad <> "b") i
                     exc = length bin - 15
                  in drop exc bin
