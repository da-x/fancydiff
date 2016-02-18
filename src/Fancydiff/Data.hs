{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS -funbox-strict-fields      #-}

module Fancydiff.Data
    ( Element(..)
    , Format(..)
    , Palette(..)
    , PaletteInt
    , ColorString(..)
    , Brightness(..)
    ) where

------------------------------------------------------------------------------------
import           Data.Text        (Text)
------------------------------------------------------------------------------------

data Element
    = Keyword
    | String
    | Number
    | Char
    | Type
    | Identifier
    | Call
    | Comment
    | DocComment
    | Special
    | Special2
    | Special3
    | Curly
    | Brackets
    | Parentheses
    | Ignore
    deriving (Show, Eq, Ord)

data Format
    = DiffMain
    | DiffMainExtra
    | DiffHunkHeader
    | DiffAdd
    | DiffRemove
    | DiffSlash
    | DiffAddFile Text
    | DiffRemoveFile Text
    | DiffUnchanged
    | DiffNothing
    | CommitMain
    | CommitMerge
    | CommitOther
    | CommitMsg
    | CommitMsgByLines
    | Mark
    | MonospacePar
    | Monospace
    | Link Text
    | Style Element
      deriving (Show, Eq, Ord)

data Brightness = P'Dark | P'Bright
    deriving (Show, Eq)

data Palette a = Palette {
      p'brightness       :: Brightness
    , p'keyword          :: !a
    , p'string           :: !a
    , p'number           :: !a
    , p'char             :: !a
    , p'type             :: !a
    , p'identifier       :: !a
    , p'call             :: !a
    , p'comment          :: !a
    , p'doccomment       :: !a
    , p'special          :: !a
    , p'special2         :: !a
    , p'special3         :: !a
    , p'curly            :: !a
    , p'brackets         :: !a
    , p'parentheses      :: !a
    , p'ignore           :: !a
    , p'commitMsgByLines :: !a
    , p'commitFG         :: !a
    , p'commitMergeFG    :: !a
    , p'commitMain       :: !a
    , p'commitOther      :: !a
    , p'diffMain         :: !a
    , p'diffMainExtra    :: !a
    , p'diffRemove       :: !a
    , p'diffAdd          :: !a
    , p'diffMarkAdd      :: !a
    , p'diffMarkRemove   :: !a
    , p'diffRemoveFile   :: !a
    , p'diffAddFile      :: !a
    , p'diffHunkHeaderBG :: !a
    , p'diffHunkHeaderFG :: !a
    , p'defaultBG        :: !a
    , p'defaultFG        :: !a
    } deriving (Show, Functor)

type PaletteInt = Palette (Int, Int, Int)

data ColorString = ColorString !Text
