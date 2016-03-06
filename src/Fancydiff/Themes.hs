{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Fancydiff.Themes where

------------------------------------------------------------------------------------
import           Fancydiff.Data
------------------------------------------------------------------------------------

darkBackground :: Palette ColorString
darkBackground = Palette {
      p'brightness       = P'Dark
    , p'keyword          = ColorString "#ffff00"
    , p'comment          = ColorString "#969896"
    , p'doccomment       = ColorString "#c66896"
    , p'string           = ColorString "#40ffff"
    , p'char             = ColorString "#cc8080"
    , p'number           = ColorString "#ff4040"
    , p'type             = ColorString "#88ff88"
    , p'identifier       = ColorString "#ffffff"
    , p'special          = ColorString "#f0e080"
    , p'special2         = ColorString "#f0c080"
    , p'special3         = ColorString "#f0a080"
    , p'call             = ColorString "#80a0a0"
    , p'toplevel         = ColorString "#80a0a0"
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
    , p'defaultBG        = ColorString "#000000"
    , p'defaultFG        = ColorString "#dddddd"
  }

brightBackground :: Palette ColorString
brightBackground = Palette {
      p'brightness       = P'Bright
    , p'keyword          = ColorString "#a71d5d"
    , p'comment          = ColorString "#666866"
    , p'doccomment       = ColorString "#552233"
    , p'string           = ColorString "#183691"
    , p'char             = ColorString "#cc8080"
    , p'number           = ColorString "#cc4040"
    , p'type             = ColorString "#0086b3"
    , p'identifier       = ColorString "#000000"
    , p'special          = ColorString "#103010"
    , p'special2         = ColorString "#105010"
    , p'special3         = ColorString "#106010"
    , p'call             = ColorString "#501050"
    , p'toplevel         = ColorString "#801080"
    , p'curly            = ColorString "#203030"
    , p'brackets         = ColorString "#205030"
    , p'parentheses      = ColorString "#305030"
    , p'ignore           = ColorString "#111111"
    , p'commitMain       = ColorString "#dd80dd"
    , p'commitFG         = ColorString "#333333"
    , p'commitMergeFG    = ColorString "#555500"
    , p'commitOther      = ColorString "#cc77cc"
    , p'commitMsgByLines = ColorString "#ffbbff"
    , p'diffMain         = ColorString "#98c0e0"
    , p'diffMainExtra    = ColorString "#98a0e0"
    , p'diffRemove       = ColorString "#ffcccc"
    , p'diffAdd          = ColorString "#ccffcc"
    , p'diffMarkRemove   = ColorString "#ff9999"
    , p'diffMarkAdd      = ColorString "#99ff99"
    , p'diffRemoveFile   = ColorString "#cccccc"
    , p'diffAddFile      = ColorString "#dddddd"
    , p'diffHunkHeaderBG = ColorString "#a9ccdd"
    , p'diffHunkHeaderFG = ColorString "#545454"
    , p'defaultBG        = ColorString "#f5f5f5"
    , p'defaultFG        = ColorString "#111111"
  }
