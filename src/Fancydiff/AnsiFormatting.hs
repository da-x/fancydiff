{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Fancydiff.AnsiFormatting where

------------------------------------------------------------------------------------
import           Data.Foldable             (toList)
import           Data.Sequence             ((><), (|>))
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import qualified Data.Text                 as T
----
import           Fancydiff.Data            (Element (Ignore, Identifier))
import           Fancydiff.Formatting
import           Fancydiff.SourceHighlight (defaultTheme)
import           Text.Printf               (printf)
------------------------------------------------------------------------------------

data FrontBack = Front | Back

trueColor :: FrontBack -> Int -> Int -> Int -> Text
trueColor fb r g b = do
    T.concat ["\x1b[", case fb of Front -> "38" ; Back -> "48", ";2;",
            T.pack (printf "%d;%d;%d" r g b),
            "m"]

elementToAnsi :: Int -> Element -> Text
elementToAnsi brightness = root
     where root = defaultTheme code

           brighter :: Int -> Int
           brighter x = x + (255 - x) `div` brightness

           code :: Int -> Int -> Int -> Text
           code r g b       = trueColor Front (brighter r) (brighter g) (brighter b)

ansiFormatting :: FList -> Text
ansiFormatting = root
    where root flist               = T.concat $ toList $ snd $ combine [] [] Nothing flist
          combine a m r flist             = foldl (crux a m) (r, Seq.empty) flist
          check a b s              = if a /= b
                                         then case a of
                                                 Nothing -> s |> "\x1b[0m\x1b[K"
                                                 Just x ->  s |> x
                                         else s

          crux _ _ (r, s) (TPlain t)   = (r, s |> plain r t)
          crux a m (r, s) (TForm f l)  = let rx        = repr a m r f

                                             s'        = check   rx  r   s
                                             (rn, es)  = combine (rx:a) (f:m) rx   l
                                             es'       = check   r   rn es

                                          in (r, s' >< es')

          plain Nothing   t        = t
          plain (Just rj) t        = case T.split (== '\n') t of
                                         [x] -> x
                                         [] -> ""
                                         (x:xs) -> T.concat $ x:(map (\y -> T.concat ["\n", rj, y]) xs)

          color b = T.concat [ "\x1b[0m", b, "\x1b[K"]

          prev [] =  ""
          prev (Nothing:xs) = prev xs
          prev (Just a:_) = a

          repr _ _ _ DiffMain           = Just $ color $ trueColor Back 0x00 0x20 0xc0
          repr _ _ _ DiffMainExtra      = Just $ color $ trueColor Back 0x00 0x20 0xa8
          repr _ _ _ DiffRemove         = Just $ color $ trueColor Back 0x40 0x00 0x00
          repr _ _ _ DiffAdd            = Just $ color $ trueColor Back 0x00 0x40 0x00

          repr _ m r Mark               = if | DiffRemove `elem` m ->
                                                 Just $ color $ trueColor Back 0x68 0x00 0x00
                                             | DiffAdd    `elem` m ->
                                                 Just $ color $ trueColor Back 0x00 0x68 0x00
                                             | otherwise           -> r

          repr _ _ _ (DiffRemoveFile _) = Just $ color $ trueColor Back 0x40 0x00 0x00
          repr _ _ _ (DiffAddFile _)    = Just $ color $ trueColor Back 0x00 0x40 0x00
          repr _ _ _ DiffHunkHeader     = Just $ color $ T.concat [trueColor Back  0x00 0x20 0x60,
                                                                   trueColor Front 0x80 0x80 0x80]
          repr _ _ _ DiffUnchanged      = Nothing
          repr _ _ _ DiffNothing        = Nothing

          repr _ _ r (Style Ignore)     = r
          repr _ _ r (Style Identifier) = r
          repr a m _ (Style e)          = if | Mark       `elem` m -> style 2
                                             | DiffRemove `elem` m -> style 3
                                             | DiffAdd    `elem` m -> style 3
                                             | otherwise           -> style 4
              where style l = Just $ T.concat ["\x1b[0m",
                                               prev a,
                                               elementToAnsi l e, "\x1b[K"]
          repr _ _ _ _                  = Nothing
