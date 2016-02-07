module Lib.Git
       (git
       , git') where

------------------------------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO)
import           Lib.Process            (readProcess)
import           Data.Text              (Text)
import           Control.Monad               (void)
------------------------------------------------------------------------------------

git :: (MonadIO m) => [Text] -> m Text
git p = readProcess "git" p

git' :: (MonadIO m) => [Text] -> m ()
git' = void . git
