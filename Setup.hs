import           Control.Monad       (when)
import           Distribution.Simple
import           System.Directory    (doesFileExist)
import           System.Process      (readProcess)
import           Data.ByteString.Char8 as BS

gitVersion :: IO ()
gitVersion = do
    let filename = "app/Internal/Version.hs"
        versionSh = "./version.sh"
    hasVersionSh <- doesFileExist versionSh
    when hasVersionSh $ do
        ver <- fmap BS.pack $ readProcess "bash" [versionSh] ""

        let override = BS.writeFile filename ver
        e <- doesFileExist filename
        if e then do orig_ver <- BS.readFile filename
                     when (ver /= orig_ver) $ do
                         override
             else override

main :: IO ()
main = gitVersion >> defaultMain
