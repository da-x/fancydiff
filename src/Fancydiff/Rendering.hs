module Fancydiff.Rendering
       ( renderPalette
       , RenderedPalette(..)
       , pick
       ) where

------------------------------------------------------------------------------------
import           Data.Text      (Text)
----
import           Fancydiff.Data (Element)
import qualified Fancydiff.Data as D
------------------------------------------------------------------------------------

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
    , p'commitMsg        :: {-# UNPACK #-} !Text
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

type ColorAccess = D.PaletteInt -> (Int, Int, Int)

renderPalette :: (ColorAccess -> Text)
                 -> (ColorAccess -> ColorAccess -> Text)
                 -> RenderedPalette
renderPalette front backAndFront = RenderedPalette
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
    , p'commitMain            = backAndFront D.p'commitMain       D.p'commitFG
    , p'commitMerge           = backAndFront D.p'commitMain       D.p'commitMergeFG
    , p'commitOther           = backAndFront D.p'commitOther      D.p'commitFG
    , p'commitMsg             = front D.p'commitFG
    , p'commitMsgByLines      = front D.p'commitMsgByLines
    , p'diffMain              = backAndFront D.p'diffMain         D.p'commitFG
    , p'diffMainExtra         = backAndFront D.p'diffMainExtra    D.p'commitFG
    , p'diffRemove            = backAndFront D.p'diffRemove       D.p'commitFG
    , p'diffAdd               = backAndFront D.p'diffAdd          D.p'commitFG
    , p'diffMarkAdd           = backAndFront D.p'diffMarkAdd      D.p'commitFG
    , p'diffMarkRemove        = backAndFront D.p'diffMarkRemove   D.p'commitFG
    , p'diffRemoveFile        = backAndFront D.p'diffRemoveFile   D.p'commitFG
    , p'diffAddFile           = backAndFront D.p'diffAddFile      D.p'commitFG
    , p'diffHunkHeader        = backAndFront D.p'diffHunkHeaderBG D.p'diffHunkHeaderFG
    }

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

