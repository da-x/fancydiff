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
import           Fancydiff.HTMLFormatting    (HTMLStyle(..))

-------------------------------------------------------------------

data OutputFormat
    = ANSI
    | HTML HTMLStyle
    | Meta

data Pager
    = Less

data Command
    = OneFile FilePath
    | Stdin (Maybe Highlighter)
    | MakeSCSS
    | Setup Bool Bool

data Opts = Opts
    { optGetVersion             :: Bool
    , optTestingMode            :: Bool
    , optTestRecursiveScan      :: Maybe FilePath
    , optTestRecursiveSkipKnown :: Bool
    , optOutputFormat           :: OutputFormat
    , optPager                  :: Maybe Pager
    , optPalette                :: (Palette ColorString)
    , optCommand                :: (Maybe Command)
    }

optsParser :: ParserInfo Opts
optsParser = info (optsParse <**> helper) idm
   where
       optsParse = do
           Opts <$> switch ( long "version" <> short 'v' <> help "Show version" )
                <*> switch ( hidden <> long "test-suite" )
                <*> optional ( strOption (long "test-recursive-scan" <> hidden ))
                <*> switch (long "test-recursive-skip-known" <> hidden )
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
                               command "make-scss" makeSCSS <>
                               command "setup" setupCmd))

       pagerArg (Just "less") = Just $ Less
       pagerArg _             = Nothing

       themeArg (Just "dark")   = darkBackground
       themeArg (Just "bright") = brightBackground
       themeArg _               = darkBackground

       formatArg (Just "ansi")        = ANSI
       formatArg (Just "html")        = HTML HTMLInline -- defaults to inline
       formatArg (Just "html-inline") = HTML HTMLInline
       formatArg (Just "html-css")    = HTML HTMLSCSS
       formatArg (Just "meta")        = Meta
       formatArg _                    = ANSI

       highlighterArg = stringToHighlighter . T.pack

       fileCmd = info (OneFile <$> (argument str (metavar "PATHNAME")))
           (progDesc "Take in a single file, given by a pathname")
       stdinCmd = info (Stdin <$> (fmap (fmap highlighterArg) $ (optional $ argument str (metavar "HIGHLIGHTER"))))
           (progDesc "Take from stdin")
       makeSCSS = info (pure MakeSCSS)
           (progDesc "Generate the SCSS for the requrested theme")
       setupCmd = info (Setup <$> (switch ( long "aliases" <> short 'a' <>
                                            help "Setup aliases instead of affecting 'log/diff/show' directly" ))
                              <*> (switch ( long "local" <> short 'l' <>
                                            help "Modify the local repo configuration only" )))
           (progDesc "Modify Git's configuration for the enablement of Fancydiff")

