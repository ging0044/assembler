module Parser ( parse
              , Instruction (..)
              , Computation (..)
              , Destination
              , Register (..)
              , Jump (..)
              ) where

import           Data.List (nub)
import           Lexer     (Token (..), join, toString)

data Instruction = AInstruction Token
                 | CInstruction
                   { destination :: [Destination]
                   , computation :: Computation
                   , jump        :: Maybe Jump
                   }
                 | LInstruction Token
                 deriving (Show, Eq)

data Register = A | M | D
              deriving (Read, Show, Eq)

type Destination = Register

data Computation = BinaryOperation Token Token Token
                 | UnaryOperation Token Token
                 | ConstantOperation Token
                 deriving (Show, Eq)

data Jump = JGT | JEQ | JGE | JLT | JNE | JLE | JMP
          deriving (Read, Show, Eq)

parse :: [[Token]] -> [Instruction]
parse = foldr parseInstruction []

parseInstruction :: [Token] -> [Instruction] -> [Instruction]
parseInstruction [] _ = error "Empty line?"
parseInstruction tokens@(t:_) acc =
  let instruction = case t of
                      (Punctuation '(') -> parseLInstruction tokens
                      AtSymbol          -> parseAInstruction tokens
                      _                 -> parseCInstruction tokens
   in instruction : acc

parseAInstruction :: [Token] -> Instruction
parseAInstruction [AtSymbol, t] =
  case t of
    Number _     -> AInstruction t
    Identifier _ -> AInstruction t
    _            -> error $ "Invalid A Instruction:\n" <> toString t
parseAInstruction t = error $ "Invalid A Instruction:\n" <> join t

parseCInstruction :: [Token] -> Instruction
parseCInstruction ts = let (dest, ts2) = maybeDestination ts
                           (tComp, tJump) = span (/= SemiColon) ts2
                           comp = getComputation tComp
                           (jump, _) = maybeJump tJump
                        in CInstruction { destination = dest
                                        , computation = comp
                                        , jump = jump
                                        }

maybeDestination :: [Token] -> ([Destination], [Token])
maybeDestination (Identifier i : EqualSign : ts) = (nub $ map (read . (:[])) i, ts)
maybeDestination ts                              = ([], ts)

getComputation :: [Token] -> Computation
getComputation [t]                     = ConstantOperation $ validateOperand t
getComputation [o@(Operator _), t]     = UnaryOperation o $ validateOperand t
getComputation [t, o@(Operator _), t2] = BinaryOperation (validateOperand t) o (validateOperand t2)

validateOperand :: Token -> Token
validateOperand t = case t of
                      n@(Number x) ->
                        if x == 0 || x == 1 || x == -1 then
                          n
                        else
                          error $ "Invalid operand: " <> show x
                      i@(Identifier [x]) ->
                        if x `elem` "ADM" then
                          i
                        else
                          error $ "Invalid operand: " <> show x

maybeJump :: [Token] -> (Maybe Jump, [Token])
maybeJump (SemiColon : Identifier i : ts) = (Just (read i), ts)
maybeJump ts                              = (Nothing, ts)

parseLInstruction :: [Token] -> Instruction
parseLInstruction ts@[Punctuation '(', t, Punctuation ')'] =
  case t of
    Identifier _ -> LInstruction t
    Number _     -> LInstruction t
    _            -> error $ "Invalid L Command: " <> join ts
parseLInstruction ts = error $ "Invalid L Command: " <> join ts
