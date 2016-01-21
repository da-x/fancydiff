module Fancydiff.Data where

data Element
    = Keyword
    | String
    | Number
    | Char
    | Type
    | Identifier
    | Call
    | FromPrelude
    | Comment
    | Special
    | Special2
    | Special3
    | Curly
    | Brackets
    | Parentheses
    | Ignore
    deriving (Show, Eq, Ord)
