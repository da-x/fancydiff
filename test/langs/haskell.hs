{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

import Something
import SomethingA as A
import qualified SomethingElse (symbol, (..))

import SomethingOther -- This is a comment

f 2 = 1
f _ = 0

y {-
   multiline comment

  x -} 2 = 1

{- |
   multiline haddock comment
   a second line
 -} 2 = 1

-- | Single haddock
x1 = False

-- | Single haddock
-- and continuation.
x2 = False

-- | Single haddock
x3 = False

-- This is a comment

main :: IO ()
main = do
   print "Hello" -- This is a comment
   print [quasi|quotation|]
   print [quasi|quota
               tion|]
   print "\\"
   return ()
