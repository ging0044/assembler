module SymbolResolver (resolveSymbols) where

import           Data.List   (foldl')
import           Data.Monoid (First (..))
import           Lexer       (Name, Token (..), toString)
import           Parser      (Instruction (..))
import           SymbolTable (Address, InstructionTable, SymbolTable (..),
                              insert, new)

initMem :: Address
initMem = 16

initInstruction :: Address
initInstruction = 0

resolveSymbols :: [Instruction] -> [Instruction]
resolveSymbols insts = let (is, tables, _) = getSymbolTable insts
                           (result, _) = foldr resolveSymbol ([], tables) is
                        in result

getSymbolTable :: [Instruction] -> ([Instruction], (SymbolTable, InstructionTable), Address)
getSymbolTable insts = let (is, itable, _) = foldl' registerLabel ([], [], initInstruction) insts
                        in foldr registerSymbol ([], (SymbolTable.new, itable), initMem) is

registerLabel :: ([Instruction], InstructionTable, Address) -> Instruction -> ([Instruction], InstructionTable, Address)
registerLabel (is, table, line) i =
  case i of
    (LInstruction (Identifier name)) ->
      let value = lookup name table
       in case value of
            Just _ -> error $ "Label defined twice: " <> name
            Nothing -> let newTable = insert table name line
                        in (is, newTable, line)
    _ -> (is <> [i], table, line + 1)

registerSymbol :: Instruction -> ([Instruction], (SymbolTable, InstructionTable), Address) -> ([Instruction], (SymbolTable, InstructionTable), Address)
registerSymbol i (is, tables, mem) =
  case i of
    (AInstruction (Identifier name)) ->
      let value = First (lookup name (snd tables)) <> First (lookup name (fst tables))
       in case value of
            First (Just _) -> (i:is, tables, mem)
            First Nothing -> let newTables = (insert (fst tables) name mem, snd tables)
                        in (i:is, newTables, mem + 1)
    _ -> (i:is, tables, mem)

resolveSymbol :: Instruction -> ([Instruction], (SymbolTable, InstructionTable)) -> ([Instruction], (SymbolTable, InstructionTable))
resolveSymbol i (is, tables) =
  case i of
    (AInstruction (Identifier name)) ->
      let value = First (lookup name (snd tables)) <> First (lookup name (fst tables))
       in case value of
        First (Just v) -> ((AInstruction $ Number v) : is, tables)
        First Nothing  -> error $ "Undeclared identifier: " <> name
    _ -> (i:is, tables)
