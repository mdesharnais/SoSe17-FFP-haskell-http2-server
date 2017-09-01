module Logger 
  ( LogLevel (..)
  , Logger (..)
  )where

import Data.Text(Text)
import qualified Data.Text.IO as TextIO

data LogLevel = Info | Crit

class Monad m => Logger m where
  log :: LogLevel -> Text -> m ()

instance Logger IO where
       log _level text = TextIO.putStrLn text
