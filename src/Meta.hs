-- | Metadata functions
module Meta where

import qualified Data.Map.Strict as Map
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory (listDirectory, renameFile)
import System.FilePath
import Types

-- |
-- Metadata db file name
traDb :: String
traDb = ".trahs.db"

-- |
-- Temporary metadata db file name
traDbTemp :: String
traDbTemp = ".trahs.db~"

-- |
-- Generates a unique replica id
mkReplicaId :: IO ReplicaId
mkReplicaId = toString <$> nextRandom

-- |
-- Generate a default global meta object
mkGlobalMeta :: IO GlobalMeta
mkGlobalMeta = GlobalMeta 0 (Map.empty) (Map.empty) <$> mkReplicaId

-- |
-- Read the global metadata from @dir@
readGlobalMeta :: FilePath -> IO GlobalMeta
readGlobalMeta dir = read <$> readFile (dir </> traDb)

-- |
-- Write the global metadata into @dir@
writeGlobalMeta :: FilePath -> GlobalMeta -> IO ()
writeGlobalMeta dir meta = do
  _ <- writeFile (dir </> traDbTemp) (show meta)
  _ <- renameFile traDbTemp traDb
  return ()
