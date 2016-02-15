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
import           Fancydiff.SourceHighlight (brighter, darker, paletteDecode)
------------------------------------------------------------------------------------

data FrontBack = Front | Back

renderPaletteForAnsi :: ((Int, Int, Int) -> (Int, Int, Int)) ->D.PaletteInt -> RenderedPalette
renderPaletteForAnsi modfunc p = renderPalette front backAndFront
    where
        wrap t = T.concat [ "\x1b[0m", t, "\x1b[K"]
        backAndFront b f =
            wrap $ T.concat [back b, front f]
        front access = code Front $ modfunc $  access p
        back access = code Back $ access p

        code :: FrontBack -> (Int, Int, Int) -> Text
        code fb (r, g, b) = do
            T.concat ["\x1b[", case fb of Front -> "38" ; Back -> "48", ";2;",
                      T.pack (printf "%d;%d;%d" r g b),
                      "m"]

ansiFormatting :: D.Palette D.ColorString -> FList -> Text
ansiFormatting palette = root
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

        colorMod =
            case D.p'brightness palette of
                D.P'Dark -> brighter
                D.P'Bright -> darker

        pal3 :: RenderedPalette
        pal3 = renderPaletteForAnsi (colorMod 0.3)  $ paletteDecode palette

        pal4 :: RenderedPalette
        pal4 = renderPaletteForAnsi (colorMod 0.25) $ paletteDecode palette

        pal :: RenderedPalette
        pal = renderPaletteForAnsi id $ paletteDecode palette

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
