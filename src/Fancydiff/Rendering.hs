module Fancydiff.Rendering
       ( renderPalette
       , RenderedPalette(..)
       , PaletteVar
       , pick
       , paletteVarText
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
    , p'toplevel         :: {-# UNPACK #-} !Text
    , p'comment          :: {-# UNPACK #-} !Text
    , p'doccomment       :: {-# UNPACK #-} !Text
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
    , p'default          :: {-# UNPACK #-} !Text
    }

data PaletteVar
    = P_keyword
    | P_string
    | P_number
    | P_char
    | P_type
    | P_identifier
    | P_call
    | P_toplevel
    | P_comment
    | P_doccomment
    | P_special
    | P_special2
    | P_special3
    | P_curly
    | P_brackets
    | P_parentheses
    | P_ignore
    | P_commitMain
    | P_commitMerge
    | P_commitOther
    | P_commitMsg
    | P_commitMsgByLines
    | P_diffMain
    | P_diffMainExtra
    | P_diffRemove
    | P_diffAdd
    | P_diffMarkAdd
    | P_diffMarkRemove
    | P_diffRemoveFile
    | P_diffAddFile
    | P_diffHunkHeader
    | P_default
    deriving (Enum, Bounded, Show)

paletteVarText :: PaletteVar -> RenderedPalette -> Text
paletteVarText pv rp = (x pv) rp
    where x P_keyword            = p'keyword
          x P_string             = p'string
          x P_number             = p'number
          x P_char               = p'char
          x P_type               = p'type
          x P_identifier         = p'identifier
          x P_call               = p'call
          x P_toplevel           = p'toplevel
          x P_comment            = p'comment
          x P_doccomment         = p'doccomment
          x P_special            = p'special
          x P_special2           = p'special2
          x P_special3           = p'special3
          x P_curly              = p'curly
          x P_brackets           = p'brackets
          x P_parentheses        = p'parentheses
          x P_ignore             = p'ignore
          x P_commitMain         = p'commitMain
          x P_commitMerge        = p'commitMerge
          x P_commitOther        = p'commitOther
          x P_commitMsg          = p'commitMsg
          x P_commitMsgByLines   = p'commitMsgByLines
          x P_diffMain           = p'diffMain
          x P_diffMainExtra      = p'diffMainExtra
          x P_diffRemove         = p'diffRemove
          x P_diffAdd            = p'diffAdd
          x P_diffMarkAdd        = p'diffMarkAdd
          x P_diffMarkRemove     = p'diffMarkRemove
          x P_diffRemoveFile     = p'diffRemoveFile
          x P_diffAddFile        = p'diffAddFile
          x P_diffHunkHeader     = p'diffHunkHeader
          x P_default            = p'default

type ColorAccess = D.PaletteInt -> (Int, Int, Int)

renderPalette :: (PaletteVar -> ColorAccess -> Text)
                 -> (PaletteVar -> ColorAccess -> ColorAccess -> Text)
                 -> RenderedPalette
renderPalette front backAndFront = RenderedPalette
    { p'keyword               = front P_keyword               D.p'keyword
    , p'string                = front P_string                D.p'string
    , p'number                = front P_number                D.p'number
    , p'char                  = front P_char                  D.p'char
    , p'type                  = front P_type                  D.p'type
    , p'identifier            = front P_identifier            D.p'identifier
    , p'call                  = front P_call                  D.p'call
    , p'toplevel              = front P_toplevel              D.p'toplevel
    , p'comment               = front P_comment               D.p'comment
    , p'doccomment            = front P_doccomment            D.p'doccomment
    , p'special               = front P_special               D.p'special
    , p'special2              = front P_special2              D.p'special2
    , p'special3              = front P_special3              D.p'special3
    , p'curly                 = front P_curly                 D.p'curly
    , p'brackets              = front P_brackets              D.p'brackets
    , p'parentheses           = front P_parentheses           D.p'parentheses
    , p'ignore                = front P_ignore                D.p'ignore
    , p'commitMain            = backAndFront P_commitMain     D.p'commitMain         D.p'commitFG
    , p'commitMerge           = backAndFront P_commitMerge    D.p'commitMain         D.p'commitMergeFG
    , p'commitOther           = backAndFront P_commitOther    D.p'commitOther        D.p'commitFG
    , p'commitMsg             = front P_commitMsg             D.p'commitFG
    , p'commitMsgByLines      = front P_commitMsgByLines      D.p'commitMsgByLines
    , p'diffMain              = backAndFront P_diffMain       D.p'diffMain           D.p'commitFG
    , p'diffMainExtra         = backAndFront P_diffMainExtra  D.p'diffMainExtra      D.p'commitFG
    , p'diffRemove            = backAndFront P_diffRemove     D.p'diffRemove         D.p'commitFG
    , p'diffAdd               = backAndFront P_diffAdd        D.p'diffAdd            D.p'commitFG
    , p'diffMarkAdd           = backAndFront P_diffMarkAdd    D.p'diffMarkAdd        D.p'commitFG
    , p'diffMarkRemove        = backAndFront P_diffMarkRemove D.p'diffMarkRemove     D.p'commitFG
    , p'diffRemoveFile        = backAndFront P_diffRemoveFile D.p'diffRemoveFile     D.p'commitFG
    , p'diffAddFile           = backAndFront P_diffAddFile    D.p'diffAddFile        D.p'commitFG
    , p'diffHunkHeader        = backAndFront P_diffHunkHeader D.p'diffHunkHeaderBG   D.p'diffHunkHeaderFG
    , p'default               = backAndFront P_default        D.p'defaultBG          D.p'defaultFG
    }

pick :: Element -> RenderedPalette -> Text
pick s pal = root s
    where
        root D.Keyword      = p'keyword pal
        root D.Comment      = p'comment pal
        root D.DocComment   = p'doccomment pal
        root D.String       = p'string pal
        root D.Char         = p'char pal
        root D.Number       = p'number pal
        root D.Type         = p'type pal
        root D.Call         = p'call pal
        root D.TopLevel     = p'toplevel pal
        root D.Special      = p'special pal
        root D.Special2     = p'special2 pal
        root D.Special3     = p'special3 pal
        root D.Parentheses  = p'parentheses pal
        root D.Brackets     = p'brackets pal
        root D.Curly        = p'curly pal
        root _              = p'ignore pal

