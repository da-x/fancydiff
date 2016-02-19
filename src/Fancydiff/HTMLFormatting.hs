{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Fancydiff.HTMLFormatting
       ( inlineHtmlFormatting
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
                                                renderPalette)
import           Fancydiff.SourceHighlight     (brighter, darker, paletteDecode)
import           Lib.DList                     (dlistConcat)
import           Lib.Text                      ((+@))
------------------------------------------------------------------------------------

data FrontBack = Front | Back

renderPaletteForInlineHTML :: ((Int, Int, Int) -> (Int, Int, Int)) -> D.PaletteInt -> RenderedPalette
renderPaletteForInlineHTML modfunc p = renderPalette front backAndFront
    where
        backAndFront b f = T.concat [back b, "; ", front f]
        front access = code Front $ modfunc $ access p
        back access = code Back $ access p

        code :: FrontBack -> (Int, Int, Int) -> Text
        code fb (r, g, b) = do
            T.concat [case fb of Front -> "color: " ;
                                 Back -> "background: ",
                      T.pack (printf "#%02x%02x%02x" r g b)]

data FormatPos = Start | End
    deriving Eq

inlineHtmlFormatting :: D.Palette D.ColorString -> Maybe (Bool -> Text -> Text) -> FList -> Text
inlineHtmlFormatting palette fileURL = root
    where root flist          = T.concat $ toList $ (before `DList.cons` (dlistConcat $ fmap (crux []) flist)) `DList.snoc` after
          crux _ (TPlain t)   = DList.singleton (plain t)
          crux s (TForm f l)  = ((html Start s f) `DList.cons` (dlistConcat (fmap (crux (f:s)) l))) `DList.snoc` (html End s f)
          delink x            = T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
          plain t             = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t
          linkStart h         = T.concat ["<a href=\"" , h, "\" style=\"text-decoration: none\">"]

          diffStartFile n t color = T.concat [maybe "" (\f -> linkStart (f n t)) fileURL,
                                            "<div style=\"", color, "\">" ]
          diffEndFile         = T.concat ["</div>", maybe "" (const "</a>") fileURL]

          before              = "<pre><div style=\"" +@ p'default pal +@ "; font-family: Monaco, 'Courier New', 'DejaVu Sans Mono', 'Bitstream Vera Sans Mono', monospace\">"
          after               = "</div></pre>"

          colorMod =
              case D.p'brightness palette of
                  D.P'Dark -> darker
                  D.P'Bright -> brighter

          pal3 :: RenderedPalette
          pal3 = renderPaletteForInlineHTML (colorMod 0.3) $ paletteDecode palette

          pal4 :: RenderedPalette
          pal4 = renderPaletteForInlineHTML (colorMod 0.25) $ paletteDecode palette

          pal :: RenderedPalette
          pal = renderPaletteForInlineHTML id $ paletteDecode palette

          html Start _ (DiffRemoveFile t) = diffStartFile False t $ p'diffRemoveFile pal
          html End   _ (DiffRemoveFile _) = diffEndFile
          html Start _ (DiffAddFile t)    = diffStartFile True t $ p'diffAddFile pal
          html End   _ (DiffAddFile _)    = diffEndFile

          html Start _ (Link t)       = linkStart t
          html End   _ (Link _)       = "</a>"

          html Start m Mark           = if | DiffRemove `elem` m -> "<span style=\"" +@ p'diffMarkRemove pal +@ ";\">"
                                           | DiffAdd    `elem` m -> "<span style=\"" +@ p'diffMarkAdd pal +@ ";\">"
                                           | otherwise           -> ""
          html End   m Mark           = if | DiffRemove `elem` m -> "</span>"
                                           | DiffAdd    `elem` m -> "</span>"
                                           | otherwise           -> ""

          html Start _ MonospacePar       = "<font size=\"3\"><div><pre style=\"line-height: 125%\">"
          html End   _ MonospacePar       = "</pre></div></font>"
          html Start _ Monospace          = "<font size=\"3\">"
          html End   _ Monospace          = "</font>"
          html Start _ CommitMsgByLines   = "<div style=\"" +@ p'commitMsgByLines pal +@ "\">"
          html Start _ CommitMsg          = "<div style=\"" +@ p'commitMsg pal +@ "\">"
          html Start _ CommitMain         = "<div style=\"" +@ p'commitMain pal +@ "\">"
          html Start _ CommitMerge        = "<div style=\"" +@ p'commitMerge pal +@ "\">"
          html Start _ CommitOther        = "<div style=\"" +@ p'commitOther pal +@ "\">"
          html End   _ CommitMsgByLines   = "</div>"
          html End   _ CommitMsg          = "</div>"
          html End   _ CommitMain         = "</div>"
          html End   _ CommitMerge        = "</div>"
          html End   _ CommitOther        = "</div>"
          html Start _ DiffMain           = "<div style=\"" +@ p'diffMain pal +@ "; font-weight: bold\">"
          html End   _ DiffMain           = "</div>"
          html Start _ DiffMainExtra      = "<div style=\"" +@ p'diffMainExtra pal +@ "\">"
          html End   _ DiffMainExtra      = "</div>"
          html Start _ DiffRemove         = "<div style=\"" +@ p'diffRemove pal +@ "\">"
          html End   _ DiffRemove         = "</div>"
          html Start _ DiffAdd            = "<div style=\"" +@ p'diffAdd pal +@ "\">"
          html End   _ DiffAdd            = "</div>"
          html Start _ DiffSlash          = "<div>"
          html End   _ DiffSlash          = "</div>"
          html Start _ DiffHunkHeader     = "<div style=\"" +@ p'diffHunkHeader pal +@ "; font-weight: bold\">"
          html End   _ DiffHunkHeader     = "</div>"
          html Start _ DiffUnchanged      = "<div>"
          html End   _ DiffUnchanged      = "</div>"
          html Start _ DiffNothing        = "<div>"
          html End   _ DiffNothing        = "</div>"
          html Start m (Style s)      = if | Mark       `elem` m -> style pal3
                                           | DiffRemove `elem` m -> style pal4
                                           | DiffAdd    `elem` m -> style pal4
                                           | otherwise           -> style pal
                where style l = "<span style=\"" +@ pick s l +@ boldness +@ "\">"
                      isBold D.Keyword  = (D.p'brightness palette == D.P'Bright)
                      isBold D.TopLevel = (D.p'brightness palette == D.P'Bright)
                      isBold _ = False
                      boldness = if isBold s then "; font-weight: bold" else ""
          html End   _ (Style _)        = "</span>"

