{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

-------------------------------------------------------------------
import qualified Control.Exception.Lifted    as E
import           Control.Monad               (when)
import qualified Data.ByteString             as B
import qualified Data.DList                  as DL
import           Data.List                   (intersperse)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Data.IORef                  (modifyIORef', newIORef, readIORef,
                                              writeIORef)
import           Control.Monad               (void)
import           Git                         (withRepository)
import           Git.Libgit2                 (lgFactory)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Options.Applicative         (execParser)
import           Data.Version                (showVersion)
import           System.Process              (createProcess, proc,
                                              CreateProcess(std_in),
                                              StdStream(..), waitForProcess,
                                              env)
import           System.Environment          (getEnvironment)
import           System.IO                   (hClose, Handle, stderr,
                                              stdin, stdout)
import           System.Exit                 (exitFailure)
import           GHC.IO.Exception            (IOErrorType(ResourceVanished), IOException(ioe_type))
----
import           Paths_fancydiff             (version)
import           Fancydiff.Lib               (tryDiffWithSourceHighlight,
                                              commitHighlight,
                                              getHighlighterFunc,
                                              getHighlighterFuncByFilename)
import           Fancydiff.Formatting        (fshow)
import           Fancydiff.AnsiFormatting    (ansiFormatting)
import           Fancydiff.HTMLFormatting    ( htmlFormatting
                                             , mkHtmlFormat
                                             , HTMLStyle(..)
                                             , HTMLFormat(fmtCSS)
                                             )
import           Lib.Text                    (safeDecode)
import           Lib.Git                     (git')
import qualified Spec as Spec
import qualified Internal.Version            as V
import           Internal.Opts
-------------------------------------------------------------------

iterHandleLines :: (MonadBaseControl IO m, MonadIO m) =>
                   Handle -> (B.ByteString -> m ()) -> m () -> m ()
iterHandleLines handle onLine onEnd = loop
  where
    loop = do
        let g = do l <- liftIO $ B.hGetLine handle
                   return $ Just l
            onErr = \(e :: E.SomeException) -> do
                when (not ("end of file" `T.isSuffixOf` (T.pack $ show e))) $ do
                    liftIO $ print e
                return $ Nothing

        r <- E.catch g onErr
        case r of
            Nothing -> onEnd
            Just line -> onLine line >> loop

groupHandleByPred :: (MonadBaseControl IO m, MonadIO m) =>
                     Handle -> (B.ByteString -> Bool) -> ([B.ByteString] -> m ()) -> m ()
groupHandleByPred handle pred' cb = do
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

    iterHandleLines handle onLine onEnd

getVersion :: T.Text
getVersion = T.concat [T.pack $ showVersion version, V.version]

mainOpts :: Opts -> IO ()
mainOpts opts@Opts{..} = do
    if | optGetVersion          -> T.putStrLn $ T.concat ["Fancydiff ", getVersion]
       | optTestingMode         -> Spec.main mainOpts opts
       | otherwise ->
         case optCommand of
             Nothing -> do T.hPutStrLn stderr $ "fancydiff: no command specified (see --help)"
                           exitFailure
             Just cmnd ->
                 case optPager of
                     Nothing -> onCmd (fmtToFunc optOutputFormat optPalette) cmnd stdout
                     Just Less -> do
                          curEnv <- getEnvironment
                          (Just handleOutToLess, _, _, handle) <-
                              createProcess (proc "less" ["-R"])
                                 {  std_in = CreatePipe
                                 ,  env = Just (("LESSANSIENDCHARS", "mK") : curEnv)
                                 }

                          let act = do onCmd (fmtToFunc optOutputFormat optPalette) cmnd handleOutToLess
                          act `E.catch` resourceVanished
                          hClose handleOutToLess `E.catch` resourceVanished
                          _ <- waitForProcess handle
                          return ()

      where fmtToFunc ANSI              theme = ansiFormatting theme
            fmtToFunc (HTML htmlstyle)  theme =
                let func fl = htmlFormatting format fl
                    format = mkHtmlFormat htmlstyle theme
                 in func
            fmtToFunc Meta       _     = fshow

            resourceVanished e =
                if ioe_type e == ResourceVanished then return () else ioError e

            formatOne fmt content outHandle func = do
                let highlighted = func (safeDecode content)
                case highlighted of
                    Left err -> do T.hPutStrLn stderr $ T.pack err
                                   exitFailure
                    Right ok -> liftIO $ T.hPutStr outHandle $ fmt ok

            onCmd _ (Setup onlyAliases isLocal) _ = do
                let git'print params = do
                        T.putStrLn $ T.concat $ ["Running: "] ++ (intersperse " " $ map onParam params)
                        git' params
                    onParam param
                        | T.filter (== ' ') param == "" = param
                        | otherwise                     = T.concat ["\"", param, "\""]
                    gitconfig p = if isLocal then git'print $ ["config"] ++ p
                                             else git'print $ ["config", "--global"] ++ p
                case onlyAliases of
                    False -> do gitconfig ["color.diff", "off"]
                                gitconfig ["pager.log", "fancydiff stdin --pager=less"]
                                gitconfig ["pager.show", "fancydiff stdin --pager=less"]
                                gitconfig ["pager.diff", "fancydiff stdin --pager=less"]

                    True ->  do gitconfig ["aliases.log-fancy",
                                           "!git -c color.diff=off -c pager.log='fancydiff stdin --pager=less' log $@"]
                                gitconfig ["aliases.show-fancy",
                                           "!git -c color.diff=off -c pager.show='fancydiff stdin --pager=less' show $@"]
                                gitconfig ["aliases.diff-fancy",
                                           "!git -c color.diff=off -c pager.diff='fancydiff stdin --pager=less' diff $@"]

                putStrLn "You are now ready to use Fancydiff"

            onCmd _ MakeSCSS outHandle = do
                case fmtCSS $ mkHtmlFormat HTMLSCSS optPalette of
                    Just css -> do
                        T.hPutStr outHandle css
                        return ()
                    Nothing -> return ()

            onCmd fmt (OneFile filepath) outHandle = do
                content <- B.readFile filepath
                formatOne fmt content outHandle (getHighlighterFuncByFilename (T.pack filepath))

            onCmd fmt (Stdin Nothing) outHandle = do
                let path = "."
                withRepository lgFactory path $ do
                    let diffStart = B.isPrefixOf "diff "
                        commitStart = B.isPrefixOf "commit "
                    groupHandleByPred stdin (\l -> diffStart l || commitStart l) $ \lineList ->
                      do let chunk = safeDecode $ B.concat $ concat $ map (\x -> [x, "\n"]) lineList
                             firstLine = head lineList

                         res <- if | diffStart firstLine   -> tryDiffWithSourceHighlight chunk
                                   | commitStart firstLine -> commitHighlight chunk
                                   | otherwise             -> tryDiffWithSourceHighlight chunk

                         liftIO $ T.hPutStr outHandle $ fmt res
                         return ()

            onCmd fmt (Stdin (Just highlighter)) outHandle = do
                content <- B.hGetContents stdin
                formatOne fmt content outHandle (getHighlighterFunc highlighter)

main :: IO ()
main = void $ execParser optsParser >>= mainOpts
