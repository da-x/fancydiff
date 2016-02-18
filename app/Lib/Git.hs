module Lib.Git
       ( git
       , git'
       ) where

------------------------------------------------------------------------------------
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import           Lib.Process            (readProcess)
------------------------------------------------------------------------------------

git :: (MonadIO m) => [Text] -> m Text
git p = readProcess "git" p

git' :: (MonadIO m) => [Text] -> m ()
git' = void . git
