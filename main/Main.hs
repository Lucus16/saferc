module Main where

import Data.Text.IO qualified as Text
import System.Environment (getArgs)
import Text.Megaparsec (parseTest)
import SaferC (file)

main :: IO ()
main = do
  [path] <- getArgs
  contents <- Text.readFile path
  parseTest file contents
