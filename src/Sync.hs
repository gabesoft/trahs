-- | Synchronization algorithm
module Sync where

import Codec.Digest.SHA
import Control.Arrow ((&&&))
import Control.Exception
import Control.Lens
import Control.Monad (guard, filterM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Meta
import System.Directory (listDirectory, renameFile)
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.PosixCompat.Files
       (isRegularFile, getSymbolicLinkStatus)
import Types

-- |
-- Perform initialization before a sync
initSync :: FilePath -> IO ()
initSync dir = do
  maybeDb <- tryJust (guard . isDoesNotExistError) (readGlobalMeta dir)
  db <- either (const mkGlobalMeta) id (return <$> maybeDb)
  meta <- updateMeta dir db
  writeGlobalMeta dir meta

-- |
-- Update the meta by scanning the contents of @dir@
updateMeta :: FilePath -> GlobalMeta -> IO GlobalMeta
updateMeta dir oldMeta = do
  let meta = oldMeta & globalVersion %~ (+ 1)
  all <- listDirectory dir
  files <- filterM canSync (fmap (dir </>) all)
  assoc <- mapM (updateFileMeta meta) files
  return (meta & fileMetaMap .~ Map.fromList assoc)

-- |
-- Update the metadata for the file at @path@
-- If an existing metadata exists and the file hasn't changed return it
-- Otherwise, create a new metadata
updateFileMeta :: GlobalMeta -> FilePath -> IO (FilePath, FileMeta)
updateFileMeta meta path = do
  sha <- hashFile path
  let fmeta = FileMeta rid vnr sha
  let emeta = maybe fmeta id (Map.lookup path fileMap)
  return
    ( path
    , if (emeta ^. fileSha) == sha
        then emeta
        else fmeta)
  where
    fileMap = meta ^. fileMetaMap
    rid = meta ^. globalReplica
    vnr = meta ^. globalVersion

-- |
-- Determine whether the file at @path@ can be synced
canSync :: FilePath -> IO Bool
canSync path = do
  isFile <- isRegularFile <$> getSymbolicLinkStatus path
  return $ notDb path && isFile
  where
    notDb = uncurry (&&) . ((/= traDb) &&& (/= traDbTemp)) . takeFileName

-- |
-- Get the max known version for a replica id
getVersion :: GlobalMeta -> ReplicaId -> VersionNr
getVersion meta rid
  | rid == (meta ^. globalReplica) = meta ^. globalVersion
  | otherwise = maybe 0 id $ Map.lookup rid (meta ^. versionVector)

-- |
-- Create a SHA-256 hash of a file
hashFile :: FilePath -> IO String
hashFile path = showBSasHex <$> (hash SHA256 <$> BL.readFile path)
