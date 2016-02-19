{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE LambdaCase                #-}

module Fancydiff.SourceHighlight
    ( nullMatcher
    , haskellMatcher
    , clangMatcher
    , paletteDecode
    , paletteEncode
    , modifyColorString
    , brighter
    , darker
    , inverseColor
    ) where

------------------------------------------------------------------------------------
import           Control.Monad              (forM_, when)
import           Control.Monad.ST           as ST
import qualified Data.ByteString            as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.DList                 (DList)
import qualified Data.DList                 as DList
import           Data.STRef                 as ST
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Numeric                    (readHex)
import           Text.Printf                (printf)
----
import           Fancydiff.Data
import           Fancydiff.Formatting       as F
import           Fancydiff.Lexer            (Token (..), TokenClass (..),
                                             alexMonadScan, alexSetStartCode,
                                             clang, haskell, runAlex)
import           Lib.Text                   (safeDecode)
------------------------------------------------------------------------------------

decodeColorString :: ColorString -> (Int, Int, Int)
decodeColorString (ColorString t) =
    let r i = fst $ head $ readHex $ T.unpack $ T.take 2 $ T.drop (1 + i*2) t
     in (r 0, r 1, r 2)

paletteDecode :: Palette ColorString -> PaletteInt
paletteDecode = fmap decodeColorString

encodeColorString :: (Int, Int, Int) -> ColorString
encodeColorString (r, g, b) = ColorString $ T.pack (printf "#%02x%02x%02x" r g b)

paletteEncode :: PaletteInt -> Palette ColorString
paletteEncode = fmap encodeColorString

modifyColorString :: ((Int, Int, Int) -> (Int, Int, Int))
                      -> ColorString -> ColorString
modifyColorString f cs =  encodeColorString $ f (decodeColorString cs)


inverseColor :: (Int, Int, Int) -> (Int, Int, Int)
inverseColor (r, g, b) = rev
    where rev = (255 - r, 255 - g, 255 - b)

brighter :: Float -> (Int, Int, Int) -> (Int, Int, Int)
brighter change (r, g, b) = (f r, f g, f b)
    where
        f x = floor $ fromIntegral x + ((255 -  fromIntegral x) * change)

darker :: Float -> (Int, Int, Int) -> (Int, Int, Int)
darker change (r, g, b) = (f r, f g, f b)
    where
        f x = floor $ fromIntegral x - ((fromIntegral x) * change)

parseWithAlex :: Int -> (DList (Int, BL8.ByteString, Element)
                         -> DList (BL8.ByteString, Element)) -> Text -> Either String FList
parseWithAlex s p t =
    let getTokens bs'     = runAlex bs' (alexSetStartCode s >> loop DList.empty)
        bs                = BL8.fromChunks [ T.encodeUtf8 t ]
        toText txt        = safeDecode $ BS8.concat $ BL8.toChunks txt
        loop s' = do
            Token cls bs' <- alexMonadScan
            case cls of
                TokenEOF -> return s'
                TokenDemark _line col e -> loop $ s' `DList.snoc` (col, bs', e)

    in case getTokens bs of
        Left err -> Left err
        Right ok -> Right $ F.fragmentize
                          $ map (\(bs', e) -> (toText bs', Just $ Style e)) $ DList.toList $ p ok

nullMatcher,
  haskellMatcher,
  clangMatcher :: Text -> Either String FList

nullMatcher t = Right $ F.highlightText t

haskellMatcher = parseWithAlex haskell (\x -> runST $ p x)
    where
        p lst = do out <- ST.newSTRef DList.empty
                   underImport <- ST.newSTRef False
                   docSingleLine <- ST.newSTRef False
                   let append i = ST.modifySTRef out (`DList.snoc` i)
                       choiceUnder opt1 opt2 under k = do
                           b <- ST.readSTRef under
                           if b
                              then append (k, opt1)
                              else append (k, opt2)
                       doccomment k = do
                           b <- ST.readSTRef docSingleLine
                           when (not b  &&   "--" `BL8.isPrefixOf` k) $ do
                               ST.writeSTRef docSingleLine True

                   forM_ lst $ \tok -> do
                       -- traceM $ show tok
                       case tok of
                           (1, k@"import",    v@Keyword)    -> ST.writeSTRef underImport True >> append (k, v)
                           (_, k@"as",        _        )    -> choiceUnder Keyword Identifier underImport k
                           (_, k@"qualified", _        )    -> choiceUnder Keyword Identifier underImport k
                           (_, k@"hiding"   , _        )    -> choiceUnder Keyword Identifier underImport k
                           (_, k            , v@DocComment) -> append (k, v) >> doccomment k
                           (_, k            , Comment)      -> choiceUnder DocComment Comment docSingleLine k
                           (1, k, Identifier)               -> ST.writeSTRef underImport False >> append (k, TopLevel)
                           (1, k, v)                        -> ST.writeSTRef underImport False >> append (k, v)
                           (_, k, v)                        -> append (k, v)

                       case tok of
                           (_, _            , DocComment)   -> return ()
                           (_, _            , Comment)      -> return ()
                           _                                -> ST.writeSTRef docSingleLine False


                   ST.readSTRef out

clangMatcher = parseWithAlex clang (\x -> runST $ p x)
    where
        p lst = do out <- ST.newSTRef DList.empty
                   lastId <- ST.newSTRef Nothing
                   let append i = ST.modifySTRef out (`DList.snoc` i)
                       cleanupLast asType = do
                           ST.readSTRef lastId >>= \case
                               Nothing -> return ()
                               Just k -> do append (k, asType)
                                            ST.writeSTRef lastId $ Nothing
                   forM_ lst $ \(_, k, v) -> do
                       case k of
                           "(" -> cleanupLast Call
                           _   -> cleanupLast Identifier
                       case v of
                           Identifier -> (ST.writeSTRef lastId $ Just k)
                           _          -> append (k, v)
                   cleanupLast Identifier
                   ST.readSTRef out
