{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Fancydiff.Formatting
       (combineFLists, FList, Fragment(..),
        Format(..), mkForm, clearFormatting,
        mkFormS, mkPlain, fshow, fragmentize,
        flistToText, highlightText, highlightMonospace,
        splitToLinesArray, flistToIList, applyIList,
        applyMarkers, makeFreeForm, combineILists) where

------------------------------------------------------------------------------------
import           Control.Monad    (when)
import           Control.Monad.ST (runST)
import qualified Data.Array       as A
import           Data.DList       (DList)
import qualified Data.DList       as DList
import           Data.Either      (isLeft, partitionEithers)
import           Data.Foldable    (toList)
import           Data.List        (groupBy)
import           Data.STRef       (modifySTRef', newSTRef, readSTRef,
                                   writeSTRef)
import           Data.Text        (Text)
import qualified Data.Text        as T
----
import           Fancydiff.Data   (Element (Ignore, FreeForm), Format (..))
import           Lib.Text         (lineSplit, subAText, textToAText, (+@))
------------------------------------------------------------------------------------

type FList = DList Fragment

data Fragment
    = TPlain {-# UNPACK #-} !Text
    | TForm  !Format FList
      deriving (Show, Eq, Ord)

toFList :: Fragment -> FList
toFList x@(TPlain _)    = DList.singleton x
toFList (TForm _ flist) = flist

class FShow a where
    fshow' :: a -> [Text]
    fshow :: a -> Text
    fshow = T.unlines . fshow'

instance FShow FList where
    fshow' lst = case concat $ map fshow' (toList lst) of
        [x] -> ["{ " +@ x +@ " }"]
        xs  -> ["{"] ++ (map ("   " +@) $ xs) ++ ["}"]

instance FShow Fragment where
    fshow' (TPlain t) = [T.pack $ show t]
    fshow' (TForm f t) =
        case fshow' t of
           []     -> [T.pack $ show f]
           (x:xs) -> (T.concat [T.pack $ show f, " ", x]):xs

instance FShow a => FShow (Either String a) where
    fshow' (Left x) = [T.pack $ show x]
    fshow' (Right x) = fshow' x

mkFormS :: Format -> FList -> FList
mkFormS f s = DList.singleton $ TForm f $ s

mkForm :: Format -> [Fragment] -> FList
mkForm f l = mkFormS f $DList.fromList l

mkPlain :: Text -> FList
mkPlain t = DList.singleton $ TPlain t

clearFormatting :: [(Text, Format)] -> FList
clearFormatting tl = DList.singleton $ TPlain $ T.concat $ map fst tl

fragmentize :: [(Text, Maybe Format)] -> FList
fragmentize = root
    where
        root l             = DList.fromList $ map g $ groupBy i l
        i (_, x) (_, y)    = x == y
        g l                =
            case snd $ head l of
                Nothing -> TPlain xl
                Just f  -> TForm f $ DList.singleton (TPlain xl)
            where xl = T.concat $ map fst l

highlightMonospace :: Text -> FList
highlightMonospace t = DList.singleton (TForm MonospacePar $ DList.singleton $ TPlain t)

highlightText :: Text -> FList
highlightText text = DList.singleton (TForm (Style Ignore) (DList.singleton $ TPlain text))

--
-- Combination examples:
--
-- ["test", X ["foo", "bar"], "x"]
-- ["te", Y ["stfoo", "ba"], "rx"]
--
-- ["st", X ["foo", "bar"], "x"]
-- [Y ["stfoo", "ba"], "rx"]
--
-- [X ["foo", "bar"], "x"]
-- [Y ["foo", "ba"], "rx"]
--
-- ["te", Y ["st"], X [ Y["foo", "ba"], "r"], "x"]
-- ["te", Y ["st",  X ["foo", "ba"]], X["r"], "x"]
--

type IList = DList FragmentI

data FragmentI
    = IPlain {-# UNPACK #-} !Int
    | IForm                 !Format IList
      deriving (Show, Eq, Ord)

flistToIList :: FList -> IList
flistToIList dl = DList.fromList $ map g $ DList.toList dl
    where g (TPlain t)  = IPlain $ T.length t
          g (TForm f l) = IForm f $ flistToIList l

applyIList :: Text -> IList -> FList
applyIList text ilist     = fst $ fr 0 id ilist
    where atext           = textToAText text
          m               = T.length text
          range !o !len   = if o > m || o + len > m then "" else subAText atext o len
                            -- ToDo: better EH
          fr :: Int -> (FList -> a) -> IList -> (a, Int)
          fr o f l        = case foldl h (DList.empty, o) l of
                                (l', eo) -> (f l', eo)

          h (l, o) u      = case g o u of
                                (r, eo) -> (l `DList.snoc` r, eo)
          g o (IPlain t)  = (TPlain $ range o t, o + t)
          g o (IForm f l) = fr o (TForm f) l

combineILists :: IList -> IList -> IList
combineILists fa' fb' = runST root where
    root = do
        let takeP inR = f 0
                where f !accum = do
                          r <- readSTRef inR
                          case r of
                              ((IPlain n):xs) -> do writeSTRef inR xs -- Removes it
                                                    f (n + accum)
                              (f'@(IForm _ _):xs) ->
                                  if accum == 0
                                       then do writeSTRef inR xs -- Removes f, return it
                                               return $ Just $ Right f'
                                       else do return $ Just $ Left accum
                              [] ->
                                  if accum == 0
                                       then return $ Nothing
                                       else return $ Just $ Left accum

        let merge inAR inBR = do
                outR <- newSTRef $ DList.empty

                let nextA f l = do
                        altInAR <- newSTRef $ DList.toList l
                        out <- merge altInAR inBR
                        na <- readSTRef altInAR
                        when (not $ null na) $ do
                            modifySTRef' inAR ((IForm f (DList.fromList na)):)
                        modifySTRef' outR (`DList.snoc` (IForm f out))
                        loop

                    nextB f l = do
                        altInBR <- newSTRef $ DList.toList l
                        out <- merge inAR altInBR
                        nb <- readSTRef altInBR
                        when (not $ null nb) $ do
                            modifySTRef' inBR ((IForm f (DList.fromList nb)):)
                        modifySTRef' outR (`DList.snoc` (IForm f out))
                        loop

                    loop = do
                        pa <- takeP inAR
                        pb <- takeP inBR
                        case (pa, pb) of
                            (Just (Left ta),  Just (Left tb)) -> do
                                modifySTRef' outR (`DList.snoc` (IPlain (min ta tb)))
                                if | ta > tb   -> modifySTRef' inAR ((IPlain $ ta - tb):)
                                   | ta < tb   -> modifySTRef' inBR ((IPlain $ tb - ta):)
                                   | otherwise -> return ()
                                loop

                            (Nothing,         Nothing)              -> return ()

                            (Just (Left ta),  Nothing)              -> modifySTRef' inAR (IPlain ta:)
                            (Nothing,         Just (Left tb))       -> modifySTRef' inBR (IPlain tb:)
                            (Just (Right a),  Nothing)              -> modifySTRef' inAR (a:)
                            (Nothing,         Just (Right b))       -> modifySTRef' inBR (b:)

                            (Just (Right (IForm f l)), Just (Left tb)) -> do
                                modifySTRef' inBR (IPlain tb:)
                                nextA f l
                            (Just (Left ta),           Just (Right (IForm f l))) -> do
                                modifySTRef' inAR (IPlain ta:)
                                nextB f l
                            (Just (Right (IForm f l)), Just (Right fb@(IForm _ _))) -> do
                                modifySTRef' inBR (fb:)
                                nextA f l

                            (Just (Right (IPlain _)), _)            -> return ()
                            (_, Just (Right (IPlain _)))            -> return ()

                loop

                readSTRef outR

        inAR <- newSTRef (DList.toList fa')
        inBR <- newSTRef (DList.toList fb')
        merge inAR inBR

combineFLists :: Text -> FList -> FList -> Either String FList
combineFLists t a b =
    Right $ applyIList t $ combineILists (flistToIList a) (flistToIList b)

splitToLinesArray :: FList -> A.Array Int FList
splitToLinesArray = root
    where root flist                   = A.listArray (1, n) l
               where l = case onFrag (TForm Mark flist) of
                             Left x    -> [DList.singleton x]
                             Right lst -> map snd $ reduce $ map (\(x, a) -> (x, toFList a)) $ lst
                     n = length l
                     reduce ((0, a):(x, b):xs) = reduce $ (x, (a `DList.append` b)):xs
                     reduce (x:xs)             = x : (reduce $ xs)
                     reduce xs                 = xs

          onFlist flist                = map onFrag $ toList flist
          allLefts l                   = and $ map isLeft l
          onFrag x@(TPlain t)          = case lineSplit t of
                                             [_] -> if "\n" `T.isSuffixOf` t
                                                       then Right ([(1, x)]) else Left x
                                             lst -> Right (map (\y -> (if "\n" `T.isSuffixOf` y
                                                                       then 1 else 0, TPlain y)) lst)
          onFrag x@(TForm f flist)     = crux
                  where slist = onFlist flist
                        y lr  = case partitionEithers lr of
                                   (l, []) -> [(0 :: Int, TForm f $ DList.fromList l)]
                                   ([], r) -> map (\(t,s) -> (t, TForm f $ DList.singleton s))
                                               (concat r)
                                   _ -> error "splitToLinesArray"

                        rlist = map y (groupBy (\a b -> isLeft a == isLeft b) slist)
                        crux = if allLefts slist
                                 then Left x
                                 else Right $ concat $ rlist

flistToText :: FList -> Text
flistToText = root
    where root l           = T.concat $ concat $ map crux $ toList l
          crux (TPlain t)  = [t]
          crux (TForm _ l) = concat $ map crux $ toList l

makeFreeForm :: Text -> Text -> Fragment
makeFreeForm cls t = TForm (Style (FreeForm cls)) $ DList.singleton (TPlain t)

applyMarkers :: Text -> Text -> (Text -> Fragment) -> Text -> FList
applyMarkers mStart mEnd fragMakerF t = root
    where startSplit = map (T.splitOn mEnd) $ T.splitOn mStart t
          root = DList.fromList $ concat $ map addMarker startSplit
          addMarker [a, b] = [fragMakerF a, TPlain b]
          addMarker z = map TPlain z

_test :: IO ()
_test = do
    print $ splitToLinesArray $
             DList.fromList [TForm Mark    $ DList.fromList [
                                   TForm Monospace $ DList.fromList [
                                           TPlain "bla", TPlain "hello\n" , TPlain "wo",
                                           TPlain "rld\nbla", TPlain "X", TPlain "mult\n\n",
                                           TPlain "bla" ] ] ]

    let p c' = IPlain c'
        l x = DList.fromList x
        f s l' = IForm s (l l')

        x1 = l [p 10]
        x2 = l [p 10]
        x3 = l [f Mark [p 10]]
        x4 = l [f Monospace [p 10]]
        x6 = l [p 4, f Monospace [p 4], p 2]
        x7 = l [p 5, f Mark      [p 2], p 3]
        x8 = l [p 2, f Mark      [p 5], p 3]

    print $ combineILists x1 x2
    print $ combineILists x1 x3
    print $ combineILists x4 x1
    print $ combineILists x3 x4
    print $ combineILists x1 x6
    print $ combineILists x6 x7
    print $ combineILists x7 x6
    print $ combineILists x6 x8
    print $ combineILists x8 x6
