{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Spec (main) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted  as E
import           Control.Monad               (void, forM_, when)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, evalStateT, get, MonadState,
                                              modify)
import           Control.Lens                (makeLenses)

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Typeable               (Typeable)

import           Data.List                   (intersperse)
import           Data.Text                   (Text)
import qualified Data.Text                 as T

import qualified Data.Text.IO              as T
import           System.Console.ANSI
import           System.Environment         (setEnv)
import           System.FilePath            ((</>))
import           System.Directory           (setCurrentDirectory,
                                            createDirectory, getCurrentDirectory,
                                            getDirectoryContents)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Exit                (ExitCode (..), exitWith)
import           Data.IORef
----
import           Lib.Process                (readProcess, readProcess'')
import qualified Paths_fancydiff             as Paths_fancydiff
------------------------------------------------------------------------------------

data UnexpectedState = UnexpectedState String deriving (Typeable)
instance E.Exception UnexpectedState

instance Show UnexpectedState where
  show (UnexpectedState msgstr) = "UnexpectedState: " ++ msgstr

data Context = Context {
      contextStrs     :: (IORef [Text])
    , sourceDir       :: FilePath
    , contextOutputs  :: (Maybe FilePath)
    }

makeLenses ''Context

class (MonadIO m, MonadState Context m, MonadBaseControl IO m, MonadMask m) => MonadSpec m where
instance (MonadIO m, MonadBaseControl IO m, MonadMask m) => MonadSpec (StateT Context m) where

contextNew :: (MonadIO m) => m Context
contextNew = do
    i <- liftIO $ newIORef []
    return (Context i "" Nothing)

msg :: (MonadSpec m) => Text -> m ()
msg x = do ctx <- get
           f ctx x
  where
    f (Context{contextStrs = ctx}) text = do
        lst <- liftIO $ readIORef ctx
        liftIO $ do
            setSGR [SetColor Foreground Vivid Cyan]
            T.putStr $ T.concat (intersperse ":" (reverse lst))
            setSGR [SetColor Foreground Dull Cyan]
            T.putStr $ ": "
            setSGR [Reset]
            T.putStrLn $ T.concat [text]

msgLines :: (MonadSpec m) => Text -> m ()
msgLines t = do
    forM_ (T.lines t) msg

git :: (MonadSpec m) => [Text] -> m Text
git p = do
    msg $ T.concat ["git ", (T.pack $ show p)]
    readProcess "git" p

git' :: (MonadSpec m) => [Text] -> m ()
git' = void . git

fancydiff :: (MonadSpec m) => [Text] -> m Text
fancydiff params = do
    bin <- liftIO $ fmap (</> "fancydiff") Paths_fancydiff.getBinDir
    readProcess bin params

tests :: (MonadSpec m) => FilePath -> m ()
tests tempDir = do
    Context{..} <- get

    msg "Highlighting of various source files"
    ------------------------------------------

    let langsDir = "test/langs"
    outputs <-
        case contextOutputs of
            Nothing -> E.throw $ UnexpectedState "no output dir"
            Just dir -> return dir

    let outLangsDir = outputs </> "langs"
    liftIO $ createDirectory $ outLangsDir
    files <- liftIO $ getDirectoryContents langsDir
    forM_ files $ \filename -> do
        when (not ("." `T.isPrefixOf` T.pack filename)) $ do
            let outFile = (filename ++ ".flist")
                inFile = T.pack $  langsDir </> filename

            content <- fancydiff ["--format", "meta", "file", inFile]
            liftIO $ T.writeFile (outLangsDir </> outFile) content
            ansiContent <- fancydiff ["--format", "ansi", "file", inFile]

            liftIO $ do
                T.putStrLn "----------------------------------------------------"
                T.putStrLn $    T.concat [ inFile, " -> ", T.pack outFile ]
                T.putStrLn "----------------------------------------------------"
                T.putStrLn ""
                T.putStr ansiContent

    let initRepo r = do
            liftIO $ createDirectory r
            liftIO $ setCurrentDirectory r
            git' ["init"]
        repoDir    = tempDir </> "repo"

    initRepo repoDir

    msg "Source highlight in diff"
    ------------------------------

wrap :: (MonadSpec m) => Text -> m b -> m b
wrap title x = do ctx <- get
                  f ctx title x
  where
    f (Context{contextStrs = ctx}) title' act = do
        lst <- liftIO $ readIORef ctx
        liftIO $ writeIORef ctx (title':lst)
        let restore = liftIO $ writeIORef ctx lst
            normal = do
                r <- act
                restore
                return r
            excp (e::E.SomeException) = do
                msg "aborted due to exception"
                restore
                E.throw e
        E.catch normal excp

run :: (MonadSpec m) => m ()
run = do
    wrap "main" $ do
        do help <- fancydiff ["--help"]
           wrap "help" $ msgLines help

        withSystemTempDirectory "fancydiff-test" $ \tempDir -> do
            liftIO $ setEnv "HOME" tempDir
            liftIO $ setEnv "GIT_COMMITTER_DATE" "1400000000 +0000"

            let outputDir = tempDir </> "output"

            cur <- liftIO $ getCurrentDirectory
            modify (\r -> r {contextOutputs = Just outputDir,
                             sourceDir = cur})

            liftIO $ createDirectory outputDir
            tests tempDir
            liftIO $ setCurrentDirectory cur

            let actual = T.pack outputDir
            let actualCopy = "test/actual"
            let expected = "test/expected"

            (rc, x, y) <- readProcess'' "diff" ["-ur", expected, actual] ""
            case rc of
                ExitSuccess -> do
                    msg "All seems good!"
                    return ()
                _ -> do msg "-------------------------------------------------------------------"
                        msg "Found difference between expected output and actual output"
                        msg ""
                        msgLines x
                        msgLines y
                        msg ""
                        msg $ T.concat ["Actual outputs copied to ", actualCopy, "."]
                        msgLines $ T.concat ["If they are okay, then commit them to test/expected:\n",
                                             "    rm -rf test/expected && mv test/actual test/expected && git add test/expected"]
                        _ <- readProcess "rm" ["-rf", actualCopy]
                        _ <- readProcess "cp" ["-a", actual, actualCopy]
                        liftIO $ exitWith rc

            return ()

main :: IO ()
main = do
    void $ contextNew >>= evalStateT run
