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
import           Fancydiff.Data            (Element (Ignore, Identifier))
import qualified Fancydiff.Data            as D
import           Fancydiff.Formatting
import           Fancydiff.SourceHighlight (brighter, paletteDecode)
import qualified Fancydiff.Themes          as Themes
------------------------------------------------------------------------------------

data FrontBack = Front | Back

-- Unpacked, since it is accessed a lot.
data RenderedPalette = RenderedPalette {
      p'keyword          :: {-# UNPACK #-} !Text
    , p'string           :: {-# UNPACK #-} !Text
    , p'number           :: {-# UNPACK #-} !Text
    , p'char             :: {-# UNPACK #-} !Text
    , p'type             :: {-# UNPACK #-} !Text
    , p'identifier       :: {-# UNPACK #-} !Text
    , p'call             :: {-# UNPACK #-} !Text
    , p'comment          :: {-# UNPACK #-} !Text
    , p'special          :: {-# UNPACK #-} !Text
    , p'special2         :: {-# UNPACK #-} !Text
    , p'special3         :: {-# UNPACK #-} !Text
    , p'curly            :: {-# UNPACK #-} !Text
    , p'brackets         :: {-# UNPACK #-} !Text
    , p'parentheses      :: {-# UNPACK #-} !Text
    , p'ignore           :: {-# UNPACK #-} !Text
    , p'commitMain       :: {-# UNPACK #-} !Text
    , p'commitMerge      :: {-# UNPACK #-} !Text
    , p'commitOther      :: {-# UNPACK #-} !Text
    , p'commitMsgByLines :: {-# UNPACK #-} !Text
    , p'diffMain         :: {-# UNPACK #-} !Text
    , p'diffMainExtra    :: {-# UNPACK #-} !Text
    , p'diffRemove       :: {-# UNPACK #-} !Text
    , p'diffAdd          :: {-# UNPACK #-} !Text
    , p'diffMarkAdd      :: {-# UNPACK #-} !Text
    , p'diffMarkRemove   :: {-# UNPACK #-} !Text
    , p'diffRemoveFile   :: {-# UNPACK #-} !Text
    , p'diffAddFile      :: {-# UNPACK #-} !Text
    , p'diffHunkHeader   :: {-# UNPACK #-} !Text
    }

renderPalette :: Float -> D.PaletteInt -> RenderedPalette
renderPalette brightness p = RenderedPalette
    { p'keyword               = front D.p'keyword
    , p'string                = front D.p'string
    , p'number                = front D.p'number
    , p'char                  = front D.p'char
    , p'type                  = front D.p'type
    , p'identifier            = front D.p'identifier
    , p'call                  = front D.p'call
    , p'comment               = front D.p'comment
    , p'special               = front D.p'special
    , p'special2              = front D.p'special2
    , p'special3              = front D.p'special3
    , p'curly                 = front D.p'curly
    , p'brackets              = front D.p'brackets
    , p'parentheses           = front D.p'parentheses
    , p'ignore                = front D.p'ignore
    , p'commitMain            = wrap $ back D.p'commitMain
    , p'commitMerge           = wrap $ T.concat [back D.p'commitMain,
                                                 front D.p'commitMergeFG]
    , p'commitOther           = wrap $ back D.p'commitOther
    , p'commitMsgByLines      = front D.p'commitMsgByLines
    , p'diffMain              = wrap $ back D.p'diffMain
    , p'diffMainExtra         = wrap $ back D.p'diffMainExtra
    , p'diffRemove            = wrap $ back D.p'diffRemove
    , p'diffAdd               = wrap $ back D.p'diffAdd
    , p'diffMarkAdd           = wrap $ back D.p'diffMarkAdd
    , p'diffMarkRemove        = wrap $ back D.p'diffMarkRemove
    , p'diffRemoveFile        = wrap $ back D.p'diffRemoveFile
    , p'diffAddFile           = wrap $ back D.p'diffAddFile
    , p'diffHunkHeader        = wrap $ T.concat [back D.p'diffHunkHeaderBG,
                                                 front D.p'diffHunkHeaderFG]
    }
    where
        wrap t = T.concat [ "\x1b[0m", t, "\x1b[K"]

        front access = code Front $ brighter brightness $ access p
        back access = code Back $ access p

        code :: FrontBack -> (Int, Int, Int) -> Text
        code fb (r, g, b) = do
            T.concat ["\x1b[", case fb of Front -> "38" ; Back -> "48", ";2;",
                      T.pack (printf "%d;%d;%d" r g b),
                      "m"]

pick :: Element -> RenderedPalette -> Text
pick s pal = root s
    where
        root D.Keyword      = p'keyword pal
        root D.Comment      = p'comment pal
        root D.String       = p'string pal
        root D.Char         = p'char pal
        root D.Number       = p'number pal
        root D.Type         = p'type pal
        root D.Call         = p'call pal
        root D.Special      = p'special pal
        root D.Special2     = p'special2 pal
        root D.Special3     = p'special3 pal
        root D.Parentheses  = p'parentheses pal
        root D.Brackets     = p'brackets pal
        root D.Curly        = p'curly pal
        root _              = p'ignore pal

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
        pal3 = renderPalette 0.3  $ paletteDecode Themes.darkBackground

        pal4 :: RenderedPalette
        pal4 = renderPalette 0.25 $ paletteDecode Themes.darkBackground

        pal :: RenderedPalette
        pal = renderPalette 0 $ paletteDecode Themes.darkBackground

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
        repr _ _ _ CommitMain         = Just $ p'commitMain pal
        repr _ _ _ CommitMerge        = Just $ p'commitMerge pal
        repr _ _ _ CommitOther        = Just $ p'commitOther pal

        repr _ _ r (Style Ignore)     = r
        repr _ _ r (Style Identifier) = r
        repr a m _ (Style e)          = if | Mark       `elem` m -> style pal3
                                           | DiffRemove `elem` m -> style pal4
                                           | DiffAdd    `elem` m -> style pal4
                                           | otherwise           -> style pal
            where style l = Just $ T.concat ["\x1b[0m", prev a, pick e l, "\x1b[K"]
        repr _ _ _ _                  = Nothing
