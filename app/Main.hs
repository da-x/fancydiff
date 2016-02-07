{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}

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
import           Options.Applicative         (command, info, (<**>),
                                              argument, str, metavar, ParserInfo,
                                              helper, idm, optional, progDesc,
                                              switch, long, short, strOption,
                                              help, execParser, (<>), hidden,
                                              hsubparser)
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
                                              getHighlighterByFilename,
                                              Highlighter(..),
                                              stringToHighlighter)
import           Fancydiff.Formatting        (fshow)
import           Fancydiff.AnsiFormatting    (ansiFormatting)
import           Lib.Text                    (safeDecode)
import           Lib.Git                     (git')
import qualified Spec as Spec
import qualified Internal.Version            as V
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

data OutputFormat
    = ANSI
    | Meta

data Pager
    = Less

data Command
    = OneFile FilePath
    | Stdin (Maybe Highlighter)
    | Setup Bool Bool

data Opts
    = Opts Bool Bool OutputFormat (Maybe Pager) (Maybe Command)

getVersion :: T.Text
getVersion = T.concat [T.pack $ showVersion version, V.version]

optsParser :: ParserInfo Opts
optsParser = info (optsParse <**> helper) idm
   where
       optsParse =
           Opts <$> switch ( long "version" <> short 'v' <> help "Show version" )
                <*> switch ( hidden <> long "test-suite" )
                <*> fmap formatArg (optional (strOption (long
                         "format" <> short 'f'
                                 <> help "Output format (defaults to ANSI codes)" )))
                <*> fmap pagerArg (optional (strOption (long
                         "pager" <> short 'p'
                                 <> help "Execute the given pager (currently supporting 'less')" )))
                <*> optional (hsubparser
                              (command "file" fileCmd <>
                               command "stdin" stdinCmd <>
                               command "setup" setupCmd))

       pagerArg (Just "less") = Just $ Less
       pagerArg _             = Nothing

       formatArg (Just "ansi") = ANSI
       formatArg (Just "meta") = Meta
       formatArg _             = ANSI

       highlighterArg = stringToHighlighter . T.pack

       fileCmd = info (OneFile <$> (argument str (metavar "PATHNAME")))
           (progDesc "Take in a single file, given by a pathname")
       stdinCmd = info (Stdin <$> (fmap (fmap highlighterArg) $ (optional $ argument str (metavar "HIGHLIGHTER"))))
           (progDesc "Take from stdin")
       setupCmd = info (Setup <$> (switch ( long "aliases" <> short 'a' <>
                                            help "Setup aliases instead of affecting 'log/diff/show' directly" ))
                              <*> (switch ( long "local" <> short 'l' <>
                                            help "Modify the local repo configuration only" )))
           (progDesc "Modify Git's configuration for the enablement of Fancydiff")

main :: IO ()
main = do
    void $ execParser optsParser >>= \case
        Opts True _ _ _ _ -> do
            T.putStrLn $ T.concat ["Fancydiff ", getVersion]

        Opts _ True _ _ _ -> do
            Spec.main

        Opts _ _ _ _ Nothing -> do
            T.hPutStrLn stderr $ "fancydiff: no command specified (see --help)"
            exitFailure

        Opts _ _ fmt (Just Less) (Just cmnd) -> do
            curEnv <- getEnvironment
            (Just handleOutToLess, _, _, handle) <-
                createProcess (proc "less" ["-R"])
                   {  std_in = CreatePipe
                   ,  env = Just (("LESSANSIENDCHARS", "mK") : curEnv)
                   }

            let act = do onCmd (fmtToFunc fmt) cmnd handleOutToLess
            act `E.catch` resourceVanished
            hClose handleOutToLess `E.catch` resourceVanished
            _ <- waitForProcess handle
            return ()

        Opts _ _ fmt Nothing (Just cmnd) -> do
            onCmd (fmtToFunc fmt) cmnd stdout

      where fmtToFunc ANSI = ansiFormatting
            fmtToFunc Meta = fshow

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

            onCmd fmt (OneFile filepath) outHandle = do
                content <- B.readFile filepath
                formatOne fmt content outHandle (getHighlighterByFilename (T.pack filepath))

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
