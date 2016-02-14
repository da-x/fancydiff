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
import           Fancydiff.Data                (Element (Ignore, Identifier))
import qualified Fancydiff.Data                as D
import           Fancydiff.Formatting
import           Fancydiff.Rendering           (RenderedPalette (..), pick,
                                                renderPalette)
import           Fancydiff.SourceHighlight     (brighter, paletteDecode)
import qualified Fancydiff.Themes              as Themes
import           Lib.DList                     (dlistConcat)
import           Lib.Text                      (showT, (+@))
------------------------------------------------------------------------------------

data FrontBack = Front | Back

renderPaletteForInlineHTML :: Float -> D.PaletteInt -> RenderedPalette
renderPaletteForInlineHTML brightness p = renderPalette front backAndFront
    where
        backAndFront b f = T.concat [back b, "; ", front f]
        front access = code Front $ brighter brightness $ access p
        back access = code Back $ access p

        code :: FrontBack -> (Int, Int, Int) -> Text
        code fb (r, g, b) = do
            T.concat [case fb of Front -> "color: " ;
                                 Back -> "background: ",
                      T.pack (printf "#%02x%02x%02x" r g b)]

data FormatPos = Start | End
    deriving Eq

inlineHtmlFormatting :: Maybe (Bool -> Text -> Text) -> FList -> Text
inlineHtmlFormatting fileURL = root
    where root flist          = T.concat $ toList $ dlistConcat $ fmap (crux []) flist
          crux _ (TPlain t)   = DList.singleton (plain t)
          crux s (TForm f l)  = ((html Start s f) `DList.cons` (dlistConcat (fmap (crux (f:s)) l))) `DList.snoc` (html End s f)
          delink x            = T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
          plain t             = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t
          linkStart h         = T.concat ["<a href=\"" , h, "\" style=\"text-decoration: none\">"]

          diffStartFile n t color = T.concat [maybe "" (\f -> linkStart (f n t)) fileURL,
                                            "<div style=\"", color, "; font-family: monospace\">" ]
          diffEndFile             = T.concat ["</div>", maybe "" (const "</a>") fileURL]

          pal3 :: RenderedPalette
          pal3 = renderPaletteForInlineHTML 0.3  $ paletteDecode Themes.darkBackground

          pal4 :: RenderedPalette
          pal4 = renderPaletteForInlineHTML 0.25 $ paletteDecode Themes.darkBackground

          pal :: RenderedPalette
          pal = renderPaletteForInlineHTML 0 $ paletteDecode Themes.darkBackground

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
          html Start _ Monospace          = "<font size=\"3\"><span style=\"font-family: monospace\">"
          html End   _ Monospace          = "</span></font>"
          html Start _ CommitMsgByLines   = "<div style=\"" +@ p'commitMsgByLines pal +@ "; font-family: monospace\">"
          html Start _ CommitMsg          = "<div style=\"" +@ p'commitMsg pal +@ "; font-family: monospace\">"
          html Start _ CommitMain         = "<div style=\"" +@ p'commitMain pal +@ "; font-family: monospace\">"
          html Start _ CommitMerge        = "<div style=\"" +@ p'commitMerge pal +@ "; font-family: monospace\">"
          html Start _ CommitOther        = "<div style=\"" +@ p'commitOther pal +@ "; font-family: monospace\">"
          html End   _ CommitMsgByLines   = "</div>"
          html End   _ CommitMsg          = "</div>"
          html End   _ CommitMain         = "</div>"
          html End   _ CommitMerge        = "</div>"
          html End   _ CommitOther        = "</div>"
          html Start _ DiffMain           = "<div style=\"" +@ p'diffMain pal +@ "; font-weight: bold; font-family: monospace\">"
          html End   _ DiffMain           = "</div>"
          html Start _ DiffMainExtra      = "<div style=\"" +@ p'diffMainExtra pal +@ "; font-family: monospace\">"
          html End   _ DiffMainExtra      = "</div>"
          html Start _ DiffRemove         = "<div style=\"" +@ p'diffRemove pal +@ "; font-family: monospace\">"
          html End   _ DiffRemove         = "</div>"
          html Start _ DiffAdd            = "<div style=\"" +@ p'diffAdd pal +@ "; font-family: monospace\">"
          html End   _ DiffAdd            = "</div>"
          html Start _ DiffSlash          = "<div style=\"font-family: monospace\">"
          html End   _ DiffSlash          = "</div>"
          html Start _ DiffHunkHeader     = "<div style=\"" +@ p'diffHunkHeader pal +@ "; font-weight: bold; font-family: monospace\">"
          html End   _ DiffHunkHeader     = "</div>"
          html Start _ DiffUnchanged      = "<div style=\"font-family: monospace\">"
          html End   _ DiffUnchanged      = "</div>"
          html Start _ DiffNothing        = "<div style=\"font-family: monospace\">"
          html End   _ DiffNothing        = "</div>"
          html Start _ Underline          = "<div style=\"text-decoration: underline\">"
          html End   _ Underline          = "</div>"
          html Start _ Emphesis           = "<div style=\"font-weight: bold\">"
          html End   _ Emphesis           = "</div>"
          html Start _ List               = "<ul>"
          html End   _ List               = "</ul>"
          html Start _ ListItem           = "<li>"
          html End   _ ListItem           = "</li>"
          html _     _ (Style Identifier) = ""
          html _     _ (Style Ignore)     = ""
          html Start m (Style s)      = if | Mark       `elem` m -> style pal3
                                           | DiffRemove `elem` m -> style pal4
                                           | DiffAdd    `elem` m -> style pal4
                                           | otherwise           -> style pal
                where style l = "<span style=\"" +@ pick s l +@ "\">"
          html End   _ (Style _)        = "</span>"
          html Start _ Table            = "<blockqoute><table cellpadding=\"2\">"
          html End   _ Table            = "</table></blockqoute>"
          html Start _ (TableRow)       = "<tr>"
          html End   _ (TableRow)       = "</tr>"
          html Start _ (TableCellPad i) = "<td width=\"" +@ showT i +@ "\">"
          html End   _ (TableCellPad _) = "</td>"
          html Start _ (TableCol i)     = "<td colspan=\"" +@ showT i +@ "\">"
          html End   _ (TableCol _)     = "</td>"
          html Start _ Dark             = "<span style=\"color: #a0a0a0\">"
          html End   _ Dark             = "</span>"
          html Start _ Footer           = "<div height=\"20\">&nbsp;</div><div style=\"color: #b0b0b0; font-size: 10px\">"
          html End   _ Footer           = "</div>"

