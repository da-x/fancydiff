{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

import Something
import SomethingA as A
import qualified SomethingElse (symbol, (..))

import SomethingOther -- This is a comment

-- This is a comment

main :: IO ()
main = do
   print "Hello" -- This is a comment
   print "\\"
   return ()
