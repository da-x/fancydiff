module Fancydiff.Data where

data Element
    = Keyword
    | String
    | Number
    | Char
    | Type
    | ImportLine
    | TopLevel
    | Identifier
    | Call
    | FromPrelude
    | Comment
    | Special
    | Ignore
    deriving (Show, Eq, Ord)
