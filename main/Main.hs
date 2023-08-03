module Main where

import Data.Foldable (traverse_)
import Data.Text.IO qualified as Text
import SaferC.Check (check)
import SaferC.Error (putError)
import SaferC.Parser (file)
import SaferC.Types (Source(..), Sourced(..))
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  [path] <- getArgs
  contents <- Text.readFile path
  case parse file path contents of
    Left errs -> putStrLn $ errorBundlePretty errs
    Right defs -> case check defs of
      Left errs -> traverse_ putError errs
      Right () -> print defs
