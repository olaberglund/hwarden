{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Menu (
    Arg (..),
    Prompt (..),
    Option (..),
    dmenu,
    ToEntry (..),
    Menu (..),
) where

import           Data.Coerce (coerce)
import           Data.List   (foldl')
import           Data.Maybe  (fromMaybe, listToMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as Text
import           Prelude     hiding (log)
import           Shelly      (Sh, run, setStdin, silently)

class ToEntry a where
    toEntry :: a -> Text

instance ToEntry Text where
    toEntry = id

newtype Menu = Menu
    { unMenu ::
        Prompt ->
        [Option] ->
        Sh (Either Text Option)
    }

data Arg = ArgObscured | ArgPrompt Text
  deriving stock (Show, Eq, Ord)

newtype Prompt = Prompt {unPrompt :: [Arg]}

newtype Option = Option {unOption :: Text}
    deriving stock (Show, Eq, Ord)

dmenu :: Prompt -> [Option] -> Sh (Either Text Option)
dmenu prompt options = do
    let ls = Text.unlines (coerce options)
    let lenLines = Text.pack (show (min 24 (length (Text.lines ls))))
    let obscureColor = "#222222"

    let fromArg current arg =
            current <> case arg of
                ArgObscured -> ["-nb", obscureColor, "-nf", obscureColor]
                ArgPrompt p -> ["-p", p]

    let args = foldl' fromArg ["-i", "-l", lenLines] (unPrompt prompt)

    setStdin ls
    let cmd = run "dmenu" args
    sel <- if ArgObscured `elem` unPrompt prompt
      then silently cmd
      else cmd
    let line = fromMaybe "" $ listToMaybe (Text.lines sel)
    pure $
        if line `elem` map coerce options
            then Right (Option line)
            else Left line
