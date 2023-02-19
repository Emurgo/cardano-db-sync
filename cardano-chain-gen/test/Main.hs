import Cardano.Mock.ChainSync.Server
import Cardano.Prelude (Text)
import Control.Monad (when, (>=>))
import Data.Maybe (isNothing)
import MigrationValidations (KnownMigration (..), knownMigrations)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import qualified Test.Cardano.Db.Mock.Property.Property as Property
import qualified Test.Cardano.Db.Mock.Unit.Alonzo as Alonzo
import qualified Test.Cardano.Db.Mock.Unit.Babbage as Babbage
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Prelude

main :: IO ()
main = do
  -- If the env is not set, set it to default.
  mPgPassFile <- lookupEnv "PGPASSFILE"
  when (isNothing mPgPassFile) $ do
    currentDir <- getCurrentDirectory
    setEnv "PGPASSFILE" (currentDir </> "test/testfiles/pgpass-testing")
  withIOManager $
    tests >=> defaultMain

tests :: IOManager -> IO TestTree
tests iom = do
  pure $
    testGroup
      "cardano-chain-gen"
      [ Babbage.unitTests iom knownMigrationsPlain
      --      , testProperty "QSM" $ Property.prop_empty_blocks iom knownMigrationsPlain
      --    , Alonzo.unitTests iom knownMigrationsPlain
      ]
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations
