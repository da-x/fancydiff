{-# LANGUAGE OverloadedStrings #-}

module Fancydiff.DiffHighlight (
    highlight,
    parseDiff,
    ParsedDiff,
    DiffHeader,
    DiffContent,
    OtherContent
    ) where

------------------------------------------------------------------------------------
import qualified Data.Algorithm.Patience as DP
import qualified Data.Char               as C
import qualified Data.DList              as DList
import           Data.Foldable           (toList)
import qualified Data.List               as DL
import           Data.Text               (Text)
import qualified Data.Text               as T
----
import           Fancydiff.Formatting    (FList, Format (..), Fragment (..),
                                          flistToText, fragmentize, mkFormS)
import           Lib.DList               (dlistConcat)
import           Lib.Text                (lineSplit, removeTrailingNewLine)
------------------------------------------------------------------------------------

type OtherContent = [(Text, Format)]
type DiffHeader = [(Text, Format)]
type DiffContent = [(Text, Format)]

type ParsedDiff = [Either OtherContent (DiffHeader, DiffContent)]

parseDiff :: Text -> ParsedDiff
parseDiff text = fileGroups
    where
        parsed          = parse $ lineSplit text

        parse (x:xs) = case' "diff "          (const DiffMain      ) diff x xs $
                       else' parse x xs
        parse []     = []

        fn = removeTrailingNewLine . keepDevNull . (T.drop 4)

        keepDevNull f@"/dev/null\n" = f
        keepDevNull f = T.drop 2 f

        diff (x:xs) = case' "index "         (const DiffMainExtra) diff x xs $
                      case' "new "           (const DiffMainExtra) diff x xs $
                      case' "old "           (const DiffMainExtra) diff x xs $
                      case' "delete "        (const DiffMainExtra) diff x xs $
                      case' "copy "          (const DiffMainExtra) diff x xs $
                      case' "rename "        (const DiffMainExtra) diff x xs $
                      case' "similarity "    (const DiffMainExtra) diff x xs $
                      case' "dissimilarity " (const DiffMainExtra) diff x xs $
                      case' "--- "           (DiffRemoveFile . fn) diff x xs $
                      case' "+++ "           (DiffAddFile    . fn) hunk x xs $
                      else'                  diff x xs
        diff []     = []

        hunk (x:xs) = case' "@@ "    (const DiffHunkHeader) hunk x xs $
                      case' "-"      (const DiffRemove    ) hunk x xs $
                      case' "+"      (const DiffAdd       ) hunk x xs $
                      case' "\\"     (const DiffSlash     ) hunk x xs $
                      case' "diff "  (const DiffMain      ) diff x xs $
                      else' hunk x xs
        hunk []     = []

        -- Infra
        case' pref mark next x xs alt =
            if pref `T.isPrefixOf` x
               then (x, (mark x)):next xs
               else alt

        else' f x@"\n" xs = (x, (DiffNothing)):(f xs)
        else' f x xs    = (x, (DiffUnchanged)):(f xs)

        fileGroups      = iterFiles $ DL.groupBy g parsed
            where
                isDiffStart (DiffMain        ) = True
                isDiffStart (DiffMainExtra   ) = True
                isDiffStart (DiffAddFile    _) = True
                isDiffStart (DiffRemoveFile _) = True
                isDiffStart _                  = False

                g (_, a) (_, b) = isDiffStart a == isDiffStart b
                iterFiles (diffMeta@((_, DiffMain):_):content:xxs) =
                     Right (diffMeta, content):(iterFiles xxs)
                iterFiles (x:xxs) = (Left x):(iterFiles xxs)
                iterFiles [] = []

highlight :: Text -> FList
highlight = onlyHighlight . parseDiff

onlyHighlight :: ParsedDiff -> FList
onlyHighlight parsedDiff = mkFormS MonospacePar $ diffHighlight
    where
        parsedDiffList  = concat $ map deEither parsedDiff
            where
                deEither (Left lst)     = lst
                deEither (Right (r, l)) = r ++ l

        diffHighlight   = intraLineDiff $ fragmentize' $ parsedDiffList
        fragmentize'    = fragmentize . (map (\(x, y) ->(x, Just y)))

        intraLineDiff :: (Foldable r) => r Fragment -> FList
        intraLineDiff = root'
            where
                root' x = DList.fromList $ iterLines $ toList x
                iterLines []                                            = []
                iterLines ((TForm DiffRemove rs):(TForm DiffAdd as):xs) =
                      let (rs', as') = mkDiff rs as
                       in (TForm DiffRemove rs'):(TForm DiffAdd as'):(iterLines xs)
                iterLines (x:xs)                                        = x:(iterLines xs)
                tokenize t = T.groupBy sep t
                sep a b
                    | C.isAlphaNum a && C.isAlphaNum b = True
                    | otherwise                        = False
                mkDiff rt dt =
                    let rtLines                = lineSplit $ flistToText rt
                        dtLines                = lineSplit $ flistToText dt
                        zLines                 = zip rtLines dtLines
                        zDiffed                = map fDiff zLines
                        rtDiffed               = dlistConcat $ DList.fromList $ map fst zDiffed
                        dtDiffed               = dlistConcat $ DList.fromList $ map snd zDiffed
                        fDiff (a, b)           =
                            let c              = DP.diff (tokenize $ T.drop 1 a)
                                                         (tokenize $ T.drop 1 b)
                             in (fragmentize $ ("-", Nothing):old c,
                                 fragmentize $ ("+", Nothing):new c)
                        old ((DP.Old t   ):xs) = (t, Just Mark):(old xs)
                        old ((DP.Both t _):xs) = (t, Nothing     ):(old xs)
                        old ((DP.New _   ):xs) = old xs
                        old []                 = []
                        new ((DP.New t   ):xs) = (t, Just Mark):(new xs)
                        new ((DP.Both t _):xs) = (t, Nothing     ):(new xs)
                        new ((DP.Old _   ):xs) = new xs
                        new []                 = []
                        diffed = if length rtLines /= length dtLines
                                    then (rt, dt)
                                    else (rtDiffed, dtDiffed)
                     in diffed
