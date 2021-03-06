{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Fancydiff.Lib
    ( DH.highlight
    , F.ansiFormatting
    , Highlighter(..)
    , commitHighlight
    , getHighlighterFuncByFilename
    , getHighlighterByFilename
    , getHighlighterFunc
    , highlighterToString
    , stringToHighlighter
    , tryDiffWithSourceHighlight
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted  as E
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Vector               as V
import qualified Data.DList                as DList
import           Data.IORef                (newIORef, readIORef, writeIORef)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.ByteString           as B
import qualified Data.Text.Encoding        as T
import           System.FilePath           ((</>))
import           Data.ByteString (ByteString)
import           Git                       (catBlob, MonadGit, GitException(BackendError))
import qualified Git
import           Text.Regex.TDFA           ((=~))
import           Text.Regex.TDFA.Text      ()
import qualified Data.Map                   as Map
----
import qualified Fancydiff.DiffHighlight   as DH
import qualified Fancydiff.AnsiFormatting  as F
import qualified Fancydiff.Formatting      as F
import qualified Fancydiff.SourceHighlight as SH
import           Fancydiff.SourceHighlight (Highlighter, syntaxMap)
import           Fancydiff.Data            as FD
import           Lib.DList                 (dlistConcat, dlistForM)
import           Lib.Regex                 ((=~+))
import           Lib.Text                  (lineSplit, safeDecode)
------------------------------------------------------------------------------------

highlighterToString :: Highlighter -> Text
highlighterToString o = fromMaybe "generic" (Map.lookup o m)
    where m = Map.fromList $ map (\(k, (d, _, _)) -> (d, k)) $ Map.toList syntaxMap

getHighlighterByFilename :: Text -> Highlighter
getHighlighterByFilename filename = t l
    where l = concat $ map (\(_, (d, _, e)) -> map (\i -> (i, d)) e) $ Map.toList syntaxMap
          t ((ext, highlighter):xs) =
              if ext `T.isSuffixOf` filename
                  then highlighter
                  else t xs
          t [] = SH.Generic

stringToHighlighter :: Text -> Highlighter
stringToHighlighter s =
    let l = [minBound ..]
        m = zip (map highlighterToString l) l
    in fromMaybe SH.Generic (lookup s m)

getHighlighterFunc :: Highlighter -> Text -> Either String F.FList
getHighlighterFunc o = fromMaybe SH.nullMatcher (Map.lookup o m)
    where m = Map.fromList $ map (\(_, (d, f, _)) -> (d, f)) $ Map.toList syntaxMap

getHighlighterFuncByFilename :: Text -> Text -> Either String F.FList
getHighlighterFuncByFilename =
    getHighlighterFunc . getHighlighterByFilename

highlightByExtension :: Text -> Text -> F.FList
highlightByExtension filename content =
    case getHighlighterFuncByFilename filename content of
        Left _ -> F.highlightText content
        Right ok -> ok

readMaybeBlob :: (MonadGit o m, MonadIO m, MonadBaseControl IO m) => Text -> Text -> m ByteString
readMaybeBlob filename hash = do
    case (filename, hash) of
        ("/dev/null", _)                                -> return ""
        (_, "0000000000000000000000000000000000000000") -> return ""
        (_, _)                                          -> do
            let inDb = Git.parseObjOid hash >>= catBlob
                inWorkingDir = \(e :: E.SomeException) -> do
                    maybeWorkdir <- Git.getActualWorkdir
                    case maybeWorkdir of
                        Just workdir -> do
                            maybeWorkdirHash <- Git.hashWorkdirPath $ T.encodeUtf8 filename
                            case maybeWorkdirHash of
                                Just workdirHash ->
                                    case hash `T.isPrefixOf` T.pack (show workdirHash) of
                                        True -> liftIO $ B.readFile $ workdir </> T.unpack filename
                                        False -> E.throw e
                                Nothing -> do
                                    E.catch (E.throw e) $ \e2 ->
                                        case e2 of
                                            BackendError _ ->
                                                -- Submodules "files" reach this, backend
                                                -- complains the commit hashes not found.
                                                return ""
                                            _ -> E.throw e
                        Nothing -> do
                            -- Could be a sub-project not cloned yet.
                            return ""
            E.catch inDb inWorkingDir

highlightSourceInDiffFile :: (MonadGit o m, MonadIO m, MonadBaseControl IO m) => Text -> Text -> DH.DiffHeader -> DH.DiffContent -> m (Maybe F.FList)
highlightSourceInDiffFile fromBlobHash toBlobHash diffMeta content  = do
    fromFilenameI <- liftIO $ newIORef Nothing
    toFilenameI <- liftIO $ newIORef Nothing

    forM_ diffMeta $ \(_, f) ->
        case f of
            F.DiffRemoveFile fromFilename ->
                liftIO $ writeIORef fromFilenameI $ Just fromFilename
            F.DiffAddFile toFilename ->
                liftIO $ writeIORef toFilenameI $ Just toFilename
            _ -> return ()

    fromFilenameM <- liftIO $ readIORef fromFilenameI
    toFilenameM <- liftIO $ readIORef toFilenameI

    case (fromFilenameM, toFilenameM) of
        (Just fromFilename, Just toFilename) -> do
            fromB <- readMaybeBlob fromFilename fromBlobHash
            toB <- readMaybeBlob toFilename toBlobHash

            let highlightWholeBlob filename blob =
                    F.splitToLinesVector $
                           (highlightByExtension filename) (safeDecode blob)
                fromHighlighted = highlightWholeBlob fromFilename fromB
                toHighlighted = highlightWholeBlob toFilename toB

            hunkIndexesI <- liftIO $ newIORef Nothing
            content' <- dlistForM content $ \x ->
                let (def, _) = x
                    keepIt = return $ F.fragmentize [(def, Nothing)]
                    takeLine src prefix a1 b1 f = do
                        mIndexes <- liftIO $ readIORef hunkIndexesI
                        case mIndexes of
                            Just (fromIdx, toIdx) -> do
                                liftIO $ writeIORef hunkIndexesI (Just (fromIdx + a1, toIdx + b1))
                                case src V.!? (f (fromIdx, toIdx) - 1) of
                                    Just line -> return $ F.TPlain prefix `DList.cons` line
                                    Nothing -> keepIt
                            Nothing -> keepIt

                in case x of
                (t, F.DiffHunkHeader) -> do
                    let r = "^@@ -([0-9]+),[0-9]+ [+]([0-9]+),[0-9]+ @@" :: Text
                    case (t =~ r) :: [[Text]] of
                        [[_, startFromStr, startToStr]] -> do
                            -- We got local reasoning for 'read' not to fail here,
                            -- from the regex above.
                            liftIO $ writeIORef hunkIndexesI
                                (Just ((read $ T.unpack startFromStr) :: Int,
                                       (read $ T.unpack startToStr)   :: Int))
                        _ -> liftIO $ writeIORef hunkIndexesI Nothing
                    keepIt

                (_, F.DiffUnchanged) -> takeLine fromHighlighted " " 1 1 fst
                (_, F.DiffRemove)    -> takeLine fromHighlighted "-" 1 0 fst
                (_, F.DiffAdd)       -> takeLine toHighlighted   "+" 0 1 snd

                _ -> keepIt

            return $ Just ((F.clearFormatting diffMeta) `DList.append` dlistConcat content')
        _ -> return Nothing

highlightSourceInDiff :: (MonadGit o m, MonadIO m, MonadBaseControl IO m) => DH.ParsedDiff -> m F.FList
highlightSourceInDiff parsed = do
    fmap dlistConcat $ dlistForM parsed $ \case
        Left other -> return $ F.clearFormatting other
        Right (diffMeta, content) -> do
            let def = F.clearFormatting diffMeta `DList.append` F.clearFormatting content
                r = "^index ([a-f0-9]+)[.][.]([a-f0-9]+)( .*)?\n$" :: Text
                indexFind (x, _) = "index " `T.isPrefixOf` x

            case filter indexFind diffMeta of
                [(line, _)] ->
                    case ((line :: Text) =~+ r) :: [[Text]] of
                        [[_, a, b, c]] -> do
                            let modDiffMeta =
                                    map (\x -> if indexFind x
                                                  then (T.concat [
                                                      "index ", T.take 12 a,
                                                      "..", T.take 12 b, c, "\n"], snd x)
                                                  else x) diffMeta
                            x <- highlightSourceInDiffFile a b modDiffMeta content
                            return $ fromMaybe def x
                        _ -> return def
                _ -> return def

tryDiffWithSourceHighlight :: (MonadGit o m, MonadIO m, MonadBaseControl IO m) => Text -> m F.FList
tryDiffWithSourceHighlight diff = do
    sourceInDiffHighlighted <- highlightSourceInDiff (DH.parseDiff diff)
    let text = F.flistToText sourceInDiffHighlighted
        diffHighlighted = DH.highlight text
    return $ F.combineFLists text diffHighlighted sourceInDiffHighlighted

commitHighlight :: (MonadGit o m) => Text -> m F.FList
commitHighlight commit = do
    return $ DList.fromList parsed
    where
        parsed       = parse $ lineSplit commit

        parse (x:xs) = case' "commit "            (const FD.CommitMain) parse x xs $
                       case' "Merge: "            (const FD.CommitMerge) parse x xs $
                       case' "Author: "           (const FD.CommitOther) parse x xs $
                       case' "AuthorDate: "       (const FD.CommitOther) parse x xs $
                       case' "Commit: "           (const FD.CommitOther) parse x xs $
                       case' "CommitDate: "       (const FD.CommitOther) parse x xs $
                       case' "Date: "             (const FD.CommitOther) parse x xs $
                       case' "    Signed-off-by:" (const CommitMsgByLines) parse x xs $
                       else' parse x xs
        parse []     = []

        -- Infra
        case' pref mark next x xs alt =
            if pref `T.isPrefixOf` x
               then (set mark x):(next xs)
               else alt

        else' f x xs    = (set (const FD.CommitMsg) x):(f xs)
        set mark x = (F.TForm (mark x) (DList.singleton $ F.TPlain x))
