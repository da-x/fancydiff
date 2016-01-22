{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Fancydiff.SourceHighlight
    ( nullMatcher
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
