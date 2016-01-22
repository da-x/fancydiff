{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Fancydiff.SourceHighlight
    ( defaultTheme
    , nullMatcher
    , haskellMatcher
    , clangMatcher
    , paletteDecode
    , brighter
    ) where

------------------------------------------------------------------------------------
import qualified Data.ByteString            as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Numeric                    (readHex)
----
import           Fancydiff.Data
import           Fancydiff.Formatting       as F
import           Fancydiff.Lexer            (Token (..), TokenClass (..),
                                             alexMonadScan, alexSetStartCode,
                                             clang, haskell, runAlex)
------------------------------------------------------------------------------------

decodeColorString :: ColorString -> (Int, Int, Int)
decodeColorString (ColorString t) =
    let r i = fst $ head $ readHex $ T.unpack $ T.take 2 $ T.drop (1 + i*2) t
     in (r 0, r 1, r 2)

paletteDecode :: Palette ColorString -> PaletteInt
paletteDecode = fmap decodeColorString

brighter :: Float -> (Int, Int, Int) -> (Int, Int, Int)
brighter brightness (r, g, b) = (f r, f g, f b)
    where
        f x = floor $ fromIntegral x + ((255 -  fromIntegral x) * brightness)

defaultTheme :: (Int -> Int -> Int -> t) -> Element -> t
defaultTheme code = root
     where root Keyword     = code 0xff 0xff 0x00
           root Comment     = code 0x96 0x98 0x96
           root String      = code 0x50 0x90 0xff
           root Char        = code 0x30 0x80 0xd0
           root Number      = code 0xff 0x40 0x40
           root Type        = code 0xa0 0xa0 0xa0
           root Call        = code 0x00 0xa0 0xa0
           root Special     = code 0xc0 0x80 0x00
           root Special2    = code 0x80 0xc0 0x00
           root Special3    = code 0xc0 0xc0 0x00
           root Parentheses = code 0x00 0xf0 0xf0
           root Brackets    = code 0x00 0xd0 0xd0
           root Curly       = code 0x00 0xc0 0xc0
           root _           = code 0xff 0xff 0xff

parseWithAlex :: Int -> ([(BL8.ByteString, Element)] -> [(BL8.ByteString, Element)]) -> Text -> Either String FList
parseWithAlex s p t =
    let getTokens bs'     = runAlex bs' (alexSetStartCode s >> loop [])
        bs                = BL8.fromChunks [ T.encodeUtf8 t ]
        toText txt        = T.decodeUtf8 $ BS8.concat $ BL8.toChunks txt
        loop s' = do
            Token cls bs' <- alexMonadScan
            case cls of
                TokenEOF -> return s'
                TokenDemark e -> loop $ (bs', e):s'

    in case getTokens bs of
        Left err -> Left err
        Right ok -> Right $ F.fragmentize
                          $ map (\(bs', e) -> (toText bs', Just $ Style e)) $ p ok

nullMatcher,
  haskellMatcher,
  clangMatcher :: Text -> Either String FList

nullMatcher t = Right $ F.highlightText t

haskellMatcher = parseWithAlex haskell reverse

clangMatcher = parseWithAlex clang (p [])
    where p r []                            = r
          p r (("(", f):(x, Identifier):xs) = p ((x, Call):(("(", f):r)) xs
          p r (x:xs)                        = p (x:r) xs
