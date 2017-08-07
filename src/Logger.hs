module Logger where

import Data.Text(Text)

data LogLevel = Info

class Monad m => Logger m where
  log :: LogLevel -> Text -> m ()
