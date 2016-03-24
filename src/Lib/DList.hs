module Lib.DList
    ( dlistConcat
    , dlistForM
    ) where

------------------------------------------------------------------------------------
import           Control.Monad                  (foldM)
import           Data.DList                  (DList)
import qualified Data.DList                  as DList
------------------------------------------------------------------------------------

dlistConcat :: DList (DList a) -> DList a
dlistConcat = DList.concat . DList.toList

dlistMapM :: (Monad m, Foldable l) =>
           (a -> m b) -> l a -> m (DList b)
dlistMapM f = foldM item DList.empty
    where item acc x = f x >>= return . (acc `DList.snoc`)

dlistForM :: (Monad m, Foldable l) =>
           l a -> (a -> m b) -> m  (DList b)
dlistForM = flip dlistMapM
