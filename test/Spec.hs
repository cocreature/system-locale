import qualified System.Locale.ReadSpec as ReadSpec
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    ReadSpec.spec
