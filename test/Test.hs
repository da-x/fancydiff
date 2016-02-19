------------------------------------------------------------------------------------
import           System.FilePath ((</>))
----
import qualified Paths_fancydiff as Paths_fancydiff
import           System.Process  (callProcess)
------------------------------------------------------------------------------------

main :: IO ()
main = do
    fancydiff <- fmap (</> "fancydiff") Paths_fancydiff.getBinDir
    callProcess fancydiff ["--test-suite"]
