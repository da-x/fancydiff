module Internal.Opts where

-------------------------------------------------------------------
import           Options.Applicative         (command, info, (<**>),
                                              argument, str, metavar, ParserInfo,
                                              helper, idm, optional, progDesc,
                                              switch, long, short, strOption,
                                              help, (<>), hidden,
                                              hsubparser)
import qualified Data.Text                   as T
----
import           Fancydiff.Lib               (Highlighter(..), stringToHighlighter)
import           Fancydiff.Data              (Palette, ColorString)
import           Fancydiff.Themes            (darkBackground, brightBackground)
-------------------------------------------------------------------

data OutputFormat
    = ANSI
    | HTMLInline
    | Meta

data Pager
    = Less

data Command
    = OneFile FilePath
    | Stdin (Maybe Highlighter)
    | Setup Bool Bool

data Opts = Opts
    { optGetVersion        :: Bool
    , optTestingMode       :: Bool
    , optTestRecursiveScan :: Maybe FilePath
    , optOutputFormat      :: OutputFormat
    , optPager             :: Maybe Pager
    , optPalette           :: (Palette ColorString)
    , optCommand           :: (Maybe Command)
    }

optsParser :: ParserInfo Opts
optsParser = info (optsParse <**> helper) idm
   where
       optsParse = do
           Opts <$> switch ( long "version" <> short 'v' <> help "Show version" )
                <*> switch ( hidden <> long "test-suite" )
                <*> optional ( strOption (long "test-recursive-scan" <> hidden))
                <*> fmap formatArg (optional (strOption (long
                         "format" <> short 'f'
                                 <> help "Output format (defaults to ANSI codes)" )))
                <*> fmap pagerArg (optional (strOption (long
                         "pager" <> short 'p'
                                 <> help "Execute the given pager (currently supporting 'less')" )))
                <*> fmap themeArg (optional (strOption (long
                         "theme" <> short 't'
                                 <> help "Choose color theme" )))
                <*> optional (hsubparser
                              (command "file" fileCmd <>
                               command "stdin" stdinCmd <>
                               command "setup" setupCmd))

       pagerArg (Just "less") = Just $ Less
       pagerArg _             = Nothing

       themeArg (Just "dark")   = darkBackground
       themeArg (Just "bright") = brightBackground
       themeArg _               = darkBackground

       formatArg (Just "ansi") = ANSI
       formatArg (Just "html-inline") = HTMLInline
       -- ToDo:
       --
       -- formatArg (Just "html-under-css") = HTMLUnderCSS
       -- formatArg (Just "html-only-css") = HTMLCSS
       --
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

