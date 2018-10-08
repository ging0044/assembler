module Lib (go) where

import           Assembler          (assemble)
import           Control.Monad      (when)
import           Data.List          (dropWhileEnd)
import           Lexer              (Token (..), lexLine)
import           Parser             (parse)
import           SymbolResolver     (resolveSymbols)
import           System.Environment (getArgs)
import           System.Exit        (die)

go :: IO ()
go = do
    arguments <- getArgs

    when (null arguments) $ die "Please provide a filename"

    let filename = head arguments

    contents <- readFile filename

    let tokens = filter (not . null) $ map lexLine $ lines contents
        instructions = parse tokens
        resolved = resolveSymbols instructions
        assembled = map assemble resolved
        outName = dropWhileEnd (/= '.') filename <> "hack"

    putStrLn $ "Writing to " <> outName
    writeFile outName $ unlines assembled

    putStrLn "Done"
    return ()
