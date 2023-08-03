module SaferC.Error where

import SaferC.Types

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Control.Comonad (extract)
import Control.Monad.Error.Class (MonadError, throwError)
import Text.Megaparsec (SourcePos(..), unPos)

data Error
  = TypeError { typeErrorExpected :: String , typeErrorFound :: Type }
  | ShadowError (Sourced Identifier)
  | ReturnOutsideFunction
  | MissingReturnValue
  | NotDefined Identifier
  | WrongArgCount { expectedArgCount :: Int, foundArgCount :: Int }
  deriving (Show)

tshow :: Show a => a -> Text
tshow = Text.pack . show

errorShort :: Error -> Text
errorShort ReturnOutsideFunction = "return outside function"
errorShort MissingReturnValue = "missing return value"
errorShort (NotDefined (Identifier i)) = "not defined: " <> i
errorShort (WrongArgCount expected found) =
  "expected " <> tshow expected <> " args, found " <> tshow found
errorShort (ShadowError (Sourced _ name)) = "already defined: " <> tshow name
errorShort (TypeError _ _) = "unexpected type"

errorDetails :: Error -> [Text]
errorDetails (TypeError expected found) =
  [ "expected: " <> Text.pack expected
  , "   found: " <> tshow found
  ]
errorDetails (ShadowError (Sourced src _)) = [ "at: " <> tshow src ]
errorDetails _ = []

putError :: Sourced Error -> IO ()
putError (Sourced Builtin err) = print err
putError (Sourced src@(Source start end) err) = do
  Text.putStrLn ""
  Text.putStrLn $ tshow src <> ": " <> errorShort err
  Text.putStrLn $ Text.unlines $ errorDetails err
  contents <- Text.readFile $ sourceName start
  let contents' = drop (pred (unPos (sourceLine start))) $ Text.lines contents
  if sourceLine start == sourceLine end
  then do
    let line = head contents'
        (line', after) = Text.splitAt (pred (unPos (sourceColumn end))) line
        (before, target) = Text.splitAt (pred (unPos (sourceColumn start))) line'
    Text.putStrLn $ before <> mark target <> after
  else do
    let startLine = head contents'
        (before, targetStart) = Text.splitAt (pred (unPos (sourceColumn start))) startLine
        endLine = contents' !! (unPos (sourceLine end) - unPos (sourceLine start))
        (targetEnd, after) = Text.splitAt (pred (unPos (sourceColumn end))) endLine
    Text.putStrLn $ before <> mark targetStart
    putStrLn "..."
    Text.putStrLn $ mark targetEnd <> after
  putStrLn ""
  where
    mark :: Text -> Text
    mark x = "\x1b[31;4;1m" <> x <> "\x1b[0m"

typeError :: MonadError (Sourced Error) m => ExprLoc -> String -> Type -> m a
typeError expr expected found = throwError $
  Sourced (extract expr) $ TypeError expected found

shadowError :: MonadError (Sourced Error) m => Identifier -> Source -> Source -> m a
shadowError name original new = throwError $
  Sourced new $ ShadowError $ Sourced original name

checkArgCount :: MonadError (Sourced Error) m => Int -> [ExprLoc] -> m ()
checkArgCount paramCount args
  | paramCount == length args = pure ()
  | otherwise = throwError $ Sourced (mconcat $ map extract args) $
      WrongArgCount paramCount (length args)
