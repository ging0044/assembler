module Lexer (lexLine, Name, Token (..), join, toString) where

import           Data.Char (isDigit, isLetter, isSpace)
import           Data.List (isPrefixOf)

data Token = Identifier Name
           | Number Int
           | AtSymbol
           | SemiColon
           | EqualSign
           | Operator Char
           | Punctuation Char
           deriving (Show, Eq)

type Name = String

lexLine :: String -> [Token]
lexLine "" = []
lexLine s = let filterLine :: String -> String
                filterLine = breakOn "//"

                filteredLine = filterLine s

              in lex' filteredLine

lex' :: String -> [Token]
lex' [] = []
lex' s@(c:cs) | isIdentifierStart c =
                let parts = spanIdentifier s
                 in Identifier (fst parts) : lex' (snd parts)
              | isDigit c =
                let parts = spanNumber s
                 in Number (read $ fst parts) : lex' (snd parts)
              | isAtSymbol c = AtSymbol : lex' cs
              | isSemiColon c = SemiColon : lex' cs
              | isSpace c = lex' cs
              | isOperator c = Operator c : lex' cs
              | isPunctuation c = Punctuation c : lex' cs
              | isEqualSign c = EqualSign : lex' cs
              | otherwise = error $ "Unexpected character: " <> [c]

join :: [Token] -> String
join = foldr (\t s -> toString t <> s) ""

toString :: Token -> String
toString (Identifier n)  = n
toString (Number n)      = show n
toString AtSymbol        = "@"
toString SemiColon       = ";"
toString (Operator c)    = [c]
toString (Punctuation c) = [c]

breakOn :: Eq a => [a] -> [a] -> [a]
breakOn _ [] = []
breakOn [] xs = xs
breakOn sub str = break' sub str []
            where break' :: Eq a => [a] -> [a] -> [a] -> [a]
                  break' _ [] acc = acc
                  break' sub s@(x:xs) acc =
                    if sub `isPrefixOf` s
                      then acc
                      else break' sub xs (acc <> pure x)

spanIdentifier :: String -> (String, String)
spanIdentifier = span isIdentifier

spanNumber :: String -> (String, String)
spanNumber = span isDigit

isAtSymbol :: Char -> Bool
isAtSymbol = (== '@')

isColon :: Char -> Bool
isColon = (== ':')

isUnderscore :: Char -> Bool
isUnderscore = (== '_')

isDollarSign :: Char -> Bool
isDollarSign = (== '$')

isEqualSign :: Char -> Bool
isEqualSign = (== '=')

isDot :: Char -> Bool
isDot = (== '.')

isIdentifier :: Char -> Bool
isIdentifier c = isDigit c
              || isIdentifierStart c

isIdentifierStart :: Char -> Bool
isIdentifierStart c = isLetter c
                    || isColon c
                    || isUnderscore c
                    || isDollarSign c
                    || isDot c

isOperator :: Char -> Bool
isOperator c = c `elem` "+-&|!"

isPunctuation :: Char -> Bool
isPunctuation = (`elem` "()")

isSemiColon :: Char -> Bool
isSemiColon = (== ';')
