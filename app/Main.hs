{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}

module Main where

-------------------------------------------------------------------
import qualified Control.Exception.Lifted as E
import            Control.Monad     (when)
import qualified Data.ByteString    as B
import qualified Data.DList         as DL
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.IORef        (modifyIORef', newIORef, readIORef,
                                    writeIORef)
import           Git               (withRepository)
import           Git.Libgit2       (lgFactory)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
----
import           Fancydiff.Lib     (trySourceHighlight)
import           Fancydiff.AnsiFormatting    (ansiFormatting)
import           Lib.Text    (safeDecode)
-------------------------------------------------------------------

iterStdinLines :: (MonadBaseControl IO m, MonadIO m) => (B.ByteString -> m ()) -> m () -> m ()
iterStdinLines onLine onEnd = loop
  where
    loop = do
        let g = do (liftIO B.getLine) >>= onLine
                   return True
            onErr = \(e :: E.SomeException) -> do
                when (not ("end of file" `T.isSuffixOf` (T.pack $ show e))) $ do
                    liftIO $ print e
                return False

        r <- E.catch g onErr
        case r of
            False -> onEnd
            True -> loop

groupStdinByPred :: (MonadBaseControl IO m, MonadIO m) => (B.ByteString -> Bool) -> ([B.ByteString] -> m ()) -> m ()
groupStdinByPred pred' cb = do
    linesI <- liftIO $ newIORef DL.empty
    let onLine line = do
            when (pred' line) $ flush
            liftIO $ modifyIORef' linesI (flip DL.snoc line)
        onEnd = flush
        flush = do
            lines' <- fmap DL.toList $ liftIO $  readIORef linesI
            liftIO $ writeIORef linesI DL.empty
            when (not . null $ lines' ) $ do
                cb lines'

    iterStdinLines onLine onEnd

main :: IO ()
main = do
    let path = "."
    withRepository lgFactory path $ do
        groupStdinByPred (\l -> ("diff " `B.isPrefixOf` l) || ("commit " `B.isPrefixOf` l)) $ \lines' ->
          do let diff = safeDecode $ B.concat $ concat $ map (\x -> [x, "\n"]) lines'
             res <- trySourceHighlight diff
             liftIO $ T.putStr $ ansiFormatting res
             return ()
