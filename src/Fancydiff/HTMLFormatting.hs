{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Fancydiff.HTMLFormatting
       ( htmlFormatting
       , mkHtmlFormat
       , HTMLStyles(..)
       , HTMLFormat(fmtFileLinker, fmtCSS),
       ) where

------------------------------------------------------------------------------------
import qualified Data.DList                    as DList
import           Data.Foldable                 (toList)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Printf                   (printf)
----
import qualified Fancydiff.Data                as D
import           Fancydiff.Formatting
import           Fancydiff.Rendering           (RenderedPalette (..), pick,
                                                renderPalette, paletteVarText)
import           Fancydiff.SourceHighlight     (brighter, darker, paletteDecode)
import           Lib.DList                     (dlistConcat)
import           Lib.Text                      ((+@))
------------------------------------------------------------------------------------

data FrontBack = Front | Back

data FormatPos = Start | End
    deriving Eq

data PaletteMods = PaletteMods
    { pal3 :: !RenderedPalette
    , pal4 :: !RenderedPalette
    , pal  :: !RenderedPalette
    }

paletteModsToList :: PaletteMods -> [RenderedPalette]
paletteModsToList (PaletteMods a b c) = [a, b, c]

data HTMLFormat = HTMLFormat
    { fmtNormalWeight :: !PaletteMods
    , fmtBoldWeight   :: !PaletteMods
    , fmtOrigPalette  :: D.Palette D.ColorString
    , fmtDelink       :: !Bool
    , fmtFileLinker   :: Maybe (Bool -> Text -> Text)
    , fmtCSS          :: Maybe Text
    }

data HTMLStyles = HTMLInline | HTMLSCSS

mkHtmlFormat :: HTMLStyles -> D.Palette D.ColorString -> HTMLFormat
mkHtmlFormat styles palette = HTMLFormat
    { fmtNormalWeight = pm False False
    , fmtBoldWeight = pm False True
    , fmtOrigPalette = palette
    , fmtDelink = case styles of
                      HTMLInline -> True
                      _ -> False
    , fmtFileLinker = Nothing
    , fmtCSS = css
    }
    where
        colorMod =
            case D.p'brightness palette of
                D.P'Dark -> darker
                D.P'Bright -> brighter

        css =
            case styles of
                HTMLInline -> Nothing
                HTMLSCSS -> Just $ T.unlines $ concat $ map (\pal' -> map (\var -> (paletteVarText var) pal') [minBound ..]) $
                                   concat $ map paletteModsToList [pm True False, pm True True]

        pm makeCSS isBold
            = let colorAttr :: FrontBack -> (Int, Int, Int) -> Text
                  colorAttr fb (r, g, b) =
                       T.concat [ case fb of Front -> "color: " ;
                                             Back -> "background: ",
                                  T.pack (printf "#%02x%02x%02x" r g b)]
                  boldStyle = if isBold then "; font-weight: bold" else ""
                  boldClass = if isBold then "-b" else ""

                  renderInline _ modfunc p = renderPalette front backAndFront
                      where
                          backAndFront _ b f = T.concat ["style=\"", back' b, "; ", front' f, boldStyle, "\""]
                          front _ f          = T.concat ["style=\"", front' f, boldStyle, "\""]
                          front' access      = colorAttr Front $ modfunc $ access p
                          back' access       = colorAttr Back $ access p

                  renderUseCSS brightness _ _ = renderPalette front backAndFront
                      where
                          className v        = T.concat [T.pack $ show v, brightness, boldClass]
                          clsAttr v          = T.concat ["class=\"", className v, "\""]
                          backAndFront v _ _ = clsAttr v
                          front v _          = clsAttr v

                  renderToCSS brightness modfunc p = renderPalette front backAndFront
                      where
                          className v        = T.concat [T.pack $ show v, brightness, boldClass]
                          backAndFront v b f = T.concat [".", className v, " {", back' b, "; ", front' f, boldStyle, "; }"]
                          front v f          = T.concat [".", className v, " {", front' f, boldStyle, "; }"]
                          front' access      = colorAttr Front $ modfunc $ access p
                          back' access       = colorAttr Back $ access p

                  render = case (makeCSS, styles) of
                               (True, _) -> renderToCSS
                               (_, HTMLInline) -> renderInline
                               _ -> renderUseCSS

                  mkpal3 = render "-3" (colorMod 0.3) $ paletteDecode palette
                  mkpal4 = render "-4" (colorMod 0.25) $ paletteDecode palette
                  mkpal  = render "" id $ paletteDecode palette

              in PaletteMods mkpal3 mkpal4 mkpal

htmlFormatting :: HTMLFormat -> FList -> Text
htmlFormatting format = root
    where root flist          = T.concat $ toList $ (before `DList.cons` (dlistConcat $ fmap (crux []) flist)) `DList.snoc` after
          crux _ (TPlain t)   = DList.singleton (plain t)
          crux s (TForm f l)  = ((html Start s f) `DList.cons` (dlistConcat (fmap (crux (f:s)) l))) `DList.snoc` (html End s f)
          delink x            = case fmtDelink format of
                                     True -> T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
                                     False -> x
          plain t             = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t
          linkStart h         = T.concat ["<a href=\"" , h, "\" style=\"text-decoration: none\">"]

          pal' = pal $ fmtNormalWeight format
          palbold' = pal $ fmtBoldWeight format
          fileURL = fmtFileLinker format
          palette = fmtOrigPalette format

          diffStartFile n t color = T.concat [maybe "" (\f -> linkStart (f n t)) fileURL,
                                            "<div ", color, ">" ]
          diffEndFile         = T.concat ["</div>", maybe "" (const "</a>") fileURL]

          before              = "<pre><div " +@ p'default pal' +@ ">"
          after               = "</div></pre>"

          html Start _ (DiffRemoveFile t) = diffStartFile False t $ p'diffRemoveFile pal'
          html End   _ (DiffRemoveFile _) = diffEndFile
          html Start _ (DiffAddFile t)    = diffStartFile True t $ p'diffAddFile pal'
          html End   _ (DiffAddFile _)    = diffEndFile

          html Start _ (Link t)           = linkStart t
          html End   _ (Link _)           = "</a>"

          html Start m Mark               = if | DiffRemove `elem` m -> "<span " +@ p'diffMarkRemove pal' +@ ">"
                                               | DiffAdd    `elem` m -> "<span " +@ p'diffMarkAdd pal' +@ ">"
                                               | otherwise           -> ""
          html End   m Mark               = if | DiffRemove `elem` m -> "</span>"
                                               | DiffAdd    `elem` m -> "</span>"
                                               | otherwise           -> ""

          html Start _ MonospacePar       = "<font size=\"3\"><div><pre line-height: 125%\">"
          html End   _ MonospacePar       = "</pre></div></font>"
          html Start _ Monospace          = "<font size=\"3\">"
          html End   _ Monospace          = "</font>"

          html Start _ CommitMsgByLines   = "<div " +@ p'commitMsgByLines pal' +@ ">"
          html Start _ CommitMsg          = "<div " +@ p'commitMsg pal' +@ ">"
          html Start _ CommitMain         = "<div " +@ p'commitMain pal' +@ ">"
          html Start _ CommitMerge        = "<div " +@ p'commitMerge pal' +@ ">"
          html Start _ CommitOther        = "<div " +@ p'commitOther pal' +@ ">"
          html End   _ CommitMsgByLines   = "</div>"
          html End   _ CommitMsg          = "</div>"
          html End   _ CommitMain         = "</div>"
          html End   _ CommitMerge        = "</div>"
          html End   _ CommitOther        = "</div>"
          html Start _ DiffMain           = "<div " +@ p'diffMain palbold' +@ ">"
          html End   _ DiffMain           = "</div>"
          html Start _ DiffMainExtra      = "<div " +@ p'diffMainExtra pal' +@ ">"
          html End   _ DiffMainExtra      = "</div>"
          html Start _ DiffRemove         = "<div " +@ p'diffRemove pal' +@ ">"
          html End   _ DiffRemove         = "</div>"
          html Start _ DiffAdd            = "<div " +@ p'diffAdd pal' +@ ">"
          html End   _ DiffAdd            = "</div>"
          html Start _ DiffSlash          = "<div>"
          html End   _ DiffSlash          = "</div>"
          html Start _ DiffHunkHeader     = "<div " +@ p'diffHunkHeader palbold' +@ ">"
          html End   _ DiffHunkHeader     = "</div>"
          html Start _ DiffUnchanged      = "<div>"
          html End   _ DiffUnchanged      = "</div>"
          html Start _ DiffNothing        = "<div>"
          html End   _ DiffNothing        = "</div>"

          html Start m (Style s)      = if | Mark       `elem` m -> style pal3
                                           | DiffRemove `elem` m -> style pal4
                                           | DiffAdd    `elem` m -> style pal4
                                           | otherwise           -> style pal
              where style l = "<span " +@ pick s (l $ (takeWeight s) format) +@ ">"
                    takeWeight D.Keyword  = if D.p'brightness palette == D.P'Bright
                                               then fmtBoldWeight
                                               else fmtNormalWeight
                    takeWeight D.TopLevel = if D.p'brightness palette == D.P'Bright
                                               then fmtBoldWeight
                                               else fmtNormalWeight
                    takeWeight _          = fmtNormalWeight
          html End   _ (Style _)        = "</span>"

