module Lib.DList
    ( dlistConcat
    , unfoldrEDL
    , unfoldrDL
    , viewl
    , ViewL(..)
    , dlistForM
    ) where

------------------------------------------------------------------------------------
import           Control.Monad                  (foldM)
import           Data.DList                  (DList)
import qualified Data.DList                  as DList
------------------------------------------------------------------------------------

dlistConcat :: DList (DList a) -> DList a
dlistConcat = DList.concat . DList.toList

unfoldrEDL :: (b -> Either String (Maybe (b, DList a))) -> b -> (Either String (DList a), b)
unfoldrEDL f = unfoldr' DList.empty
    where unfoldr' as b = either (\s -> (Left s, b)) (z as b)  $ f b
          z as b        = maybe (Right as, b)  $ \(b', a) -> unfoldr' (as `DList.append` a) b'

unfoldrDL :: (b -> Maybe (b, DList a)) -> b -> (DList a, b)
unfoldrDL f = unfoldr' DList.empty
    where unfoldr' as b = maybe (as, b) (\(b', a) -> unfoldr' (as `DList.append` a) b') $ f b

data ViewL a = EmptyL | a :<< DList a

viewl :: DList a -> ViewL a
viewl dl =
    case DList.toList dl of
         [] -> EmptyL
         (a:xs) -> a :<< DList.fromList xs

dlistMapM :: (Monad m, Foldable l) =>
           (a -> m b) -> l a -> m (DList b)
dlistMapM f = foldM item DList.empty
    where item acc x = f x >>= return . (acc `DList.snoc`)

dlistForM :: (Monad m, Foldable l) =>
           l a -> (a -> m b) -> m  (DList b)
dlistForM = flip dlistMapM
