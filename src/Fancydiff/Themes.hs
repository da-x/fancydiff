{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Fancydiff.Themes where

------------------------------------------------------------------------------------
import           Fancydiff.Data
------------------------------------------------------------------------------------

darkBackground :: Palette ColorString
darkBackground = Palette {
      p'keyword          = ColorString "#ffff00"
    , p'comment          = ColorString "#969896"
    , p'string           = ColorString "#40ffff"
    , p'char             = ColorString "#cc8080"
    , p'number           = ColorString "#ff4040"
    , p'type             = ColorString "#88ff88"
    , p'identifier       = ColorString "#ffffff"
    , p'special          = ColorString "#f0e080"
    , p'special2         = ColorString "#f0c080"
    , p'special3         = ColorString "#f0a080"
    , p'call             = ColorString "#80a0a0"
    , p'curly            = ColorString "#c0c0c0"
    , p'brackets         = ColorString "#c0d0d0"
    , p'parentheses      = ColorString "#c0f0f0"
    , p'ignore           = ColorString "#dddddd"
    , p'commitMain       = ColorString "#702070"
    , p'commitFG         = ColorString "#eeeeee"
    , p'commitMergeFG    = ColorString "#ffff00"
    , p'commitOther      = ColorString "#502050"
    , p'commitMsgByLines = ColorString "#ffbbff"
    , p'diffMain         = ColorString "#0020a0"
    , p'diffMainExtra    = ColorString "#002080"
    , p'diffRemove       = ColorString "#481010"
    , p'diffAdd          = ColorString "#104810"
    , p'diffMarkRemove   = ColorString "#781010"
    , p'diffMarkAdd      = ColorString "#107810"
    , p'diffRemoveFile   = ColorString "#303030"
    , p'diffAddFile      = ColorString "#404040"
    , p'diffHunkHeaderBG = ColorString "#001b90"
    , p'diffHunkHeaderFG = ColorString "#a0a080"
  }
