{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib.Regex ((=~+)) where

------------------------------------------------------------------------------------
import           Text.Regex.TDFA.Text        ()
import           Text.Regex.TDFA.Common      (Regex, CompOption(..), ExecOption(..))
import           Text.Regex.Base             (RegexMaker, RegexContext, makeRegexOpts,
                                              match)
------------------------------------------------------------------------------------

type RegexE a = RegexMaker Regex CompOption ExecOption a

compile' :: RegexE a => a -> Regex
compile' r = let make :: RegexE a => a -> Regex
                 make = makeRegexOpts compOpts execOpts
                 execOpts = ExecOption {captureGroups = True}
                 compOpts = CompOption {caseSensitive = True,
                                        multiline = False,
                                        rightAssoc = True,
                                        newSyntax = True,
                                        lastStarGreedy = False}
               in make r

(=~+) :: (RegexE a, RegexContext Regex source target) =>
         source -> a -> target
t =~+ r = (match . compile') r t
