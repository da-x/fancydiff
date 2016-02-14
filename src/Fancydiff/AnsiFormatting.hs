{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Fancydiff.AnsiFormatting
       (ansiFormatting)
       where

------------------------------------------------------------------------------------
import           Data.Foldable             (toList)
import           Data.Sequence             ((><), (|>))
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Text.Printf               (printf)
----
import qualified Fancydiff.Data            as D
import           Fancydiff.Formatting
import           Fancydiff.Rendering       (renderPalette, RenderedPalette(..), pick)
import           Fancydiff.SourceHighlight (brighter, paletteDecode)
import qualified Fancydiff.Themes          as Themes
------------------------------------------------------------------------------------

data FrontBack = Front | Back

renderPaletteForAnsi :: Float -> D.PaletteInt -> RenderedPalette
renderPaletteForAnsi brightness p = renderPalette front backAndFront
    where
        wrap t = T.concat [ "\x1b[0m", t, "\x1b[K"]
        backAndFront b f =
            wrap $ T.concat [back b, front f]
        front access = code Front $ brighter brightness $ access p
        back access = code Back $ access p

        code :: FrontBack -> (Int, Int, Int) -> Text
        code fb (r, g, b) = do
            T.concat ["\x1b[", case fb of Front -> "38" ; Back -> "48", ";2;",
                      T.pack (printf "%d;%d;%d" r g b),
                      "m"]

ansiFormatting :: FList -> Text
ansiFormatting = root
    where
        root flist               = T.concat $ toList $ snd $ combine [] [] Nothing flist
        combine a m r flist             = foldl (crux a m) (r, Seq.empty) flist
        check a b s              = if a /= b
                                       then case a of
                                               Nothing -> s |> "\x1b[0m\x1b[K"
                                               Just x ->  s |> x
                                       else s

        crux _ _ (r, s) (TPlain t)   = (r, s |> plain r t)
        crux a m (r, s) (TForm f l)  = let rx        = repr a m r f

                                           s'        = check   rx  r   s
                                           (rn, es)  = combine (rx:a) (f:m) rx   l
                                           es'       = check   r   rn es

                                        in (r, s' >< es')

        plain Nothing   t        = t
        plain (Just rj) t        = case T.split (== '\n') t of
                                       [x] -> x
                                       [] -> ""
                                       (x:xs) -> T.concat $ x:(map (\y -> T.concat ["\n", rj, y]) xs)

        prev [] =  ""
        prev (Nothing:xs) = prev xs
        prev (Just a:_) = a

        pal3 :: RenderedPalette
        pal3 = renderPaletteForAnsi 0.3  $ paletteDecode Themes.darkBackground

        pal4 :: RenderedPalette
        pal4 = renderPaletteForAnsi 0.25 $ paletteDecode Themes.darkBackground

        pal :: RenderedPalette
        pal = renderPaletteForAnsi 0 $ paletteDecode Themes.darkBackground

        repr _ _ _ DiffMain           = Just $ p'diffMain pal
        repr _ _ _ DiffMainExtra      = Just $ p'diffMainExtra pal
        repr _ _ _ DiffRemove         = Just $ p'diffRemove pal
        repr _ _ _ DiffAdd            = Just $ p'diffAdd pal

        repr _ m r Mark               = if | DiffRemove `elem` m -> Just $ p'diffMarkRemove pal
                                           | DiffAdd    `elem` m -> Just $ p'diffMarkAdd pal
                                           | otherwise           -> r

        repr _ _ _ (DiffRemoveFile _) = Just $ p'diffRemoveFile pal
        repr _ _ _ (DiffAddFile _)    = Just $ p'diffAddFile pal
        repr _ _ _ DiffHunkHeader     = Just $ p'diffHunkHeader pal
        repr _ _ _ DiffUnchanged      = Nothing
        repr _ _ _ DiffNothing        = Nothing
        repr _ _ _ CommitMsgByLines   = Just $ p'commitMsgByLines pal
        repr _ _ _ CommitMsg          = Just $ p'commitMsg pal
        repr _ _ _ CommitMain         = Just $ p'commitMain pal
        repr _ _ _ CommitMerge        = Just $ p'commitMerge pal
        repr _ _ _ CommitOther        = Just $ p'commitOther pal

        repr a m _ (Style e)          = if | Mark       `elem` m -> style pal3
                                           | DiffRemove `elem` m -> style pal4
                                           | DiffAdd    `elem` m -> style pal4
                                           | otherwise           -> style pal
            where style l = Just $ T.concat ["\x1b[0m", prev a, pick e l, "\x1b[K"]
        repr _ _ _ _                  = Nothing
