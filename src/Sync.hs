-- | Synchronization algorithm
module Sync where

import Codec.Digest.SHA
import Control.Arrow ((&&&))
import Control.Exception
import Control.Lens
import Control.Monad (guard, filterM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
import Meta
import System.Directory (listDirectory)
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
-- Merge a remote metadata into the local one
mergeMeta :: GlobalMeta -> GlobalMeta -> (GlobalMeta, [SyncAction])
mergeMeta localMeta remoteMeta = (meta, actions)
  where
    files = mergeFileMaps (localMeta ^. fileMetaMap) (remoteMeta ^. fileMetaMap)
    lvv = getMaxVersion localMeta
    rvv = getMaxVersion remoteMeta
    sync as path metas =
      let (a, m) = syncFiles lvv rvv path metas
      in (a : as, m)
    (actions, syncMap) = Map.mapAccumWithKey sync [] files
    fMetaMap = Map.map fromJust (Map.filter isJust syncMap)
    versions =
      mergeVersions (localMeta ^. versionVector) (remoteMeta ^. versionVector)
    meta = localMeta & versionVector .~ versions & fileMetaMap .~ fMetaMap

-- |
-- Sync two files and yield a @FileMeta@ as well as a @SyncAction@
syncFiles
  :: (FileMeta -> VersionNr)
  -> (FileMeta -> VersionNr)
  -> FilePath
  -> (Maybe FileMeta, Maybe FileMeta)
  -> (SyncAction, Maybe FileMeta)
syncFiles _ _ _ (Nothing, Nothing) = (Noop, Nothing)
syncFiles lvv rvv path (Just lm, Just rm)
  | lm == rm = (Noop, Just lm)
  | (rm ^. fileVersion) <= (lvv rm) = (Noop, Just lm)
  | (lm ^. fileVersion) <= (rvv lm) = (DownloadFile path, Just rm)
  | otherwise = (FlagConflict path (mkPath path lm) (mkPath path rm), Nothing)
syncFiles lvv _ path (Nothing, Just rm)
  | (rm ^. fileVersion) > lvv rm = (DownloadFile path, Just rm)
  | otherwise = (Noop, Nothing)
syncFiles _ rvv path (Just lm, Nothing)
  | (lm ^. fileVersion) <= rvv lm = (DeleteFile path, Nothing)
  | otherwise = (Noop, Just lm)

-- |
-- Get the max version for the file represented by @fmeta@
-- from the version vector found in @gmeta@
getMaxVersion :: GlobalMeta -> FileMeta -> VersionNr
getMaxVersion gmeta fmeta =
  maybe defaultVersion id (Map.lookup fReplica versions)
  where
    defaultVersion =
      if fReplica == gReplica
        then gmeta ^. globalVersion
        else 0
    fReplica = fmeta ^. fileReplica
    gReplica = gmeta ^. globalReplica
    versions = gmeta ^. versionVector

-- |
-- Generate a file path that contains a file's replica id and version number
mkPath :: FilePath -> FileMeta -> FilePath
mkPath base meta =
  base ++ "#" ++ (meta ^. fileReplica) ++ "." ++ (show $ meta ^. fileVersion)

-- |
-- Merge two version vector maps into another map where each replica id
-- corresponds to the max version from either map
mergeVersions :: VersionMap -> VersionMap -> VersionMap
mergeVersions = Map.unionWith max

-- |
-- Merge two file meta maps into a combined map
mergeFileMaps :: FileMetaMap
              -> FileMetaMap
              -> Map.Map FilePath (Maybe FileMeta, Maybe FileMeta)
mergeFileMaps localMetaMap remoteMetaMap =
  Map.mergeWithKey inBoth local remote localMetaMap remoteMetaMap
  where
    inBoth = (\_ a b -> Just (Just a, Just b))
    local = Map.map (\v -> (Just v, Nothing))
    remote = Map.map (\v -> (Nothing, Just v))

-- |
-- Update the meta by scanning the contents of @dir@
updateMeta :: FilePath -> GlobalMeta -> IO GlobalMeta
updateMeta dir oldMeta = do
  let meta = oldMeta & globalVersion %~ (+ 1)
  items <- listDirectory dir
  files <- filterM (canSync dir) items
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
-- Determine whether the @file@ from @dir@ can be synced
canSync :: FilePath -> FilePath -> IO Bool
canSync dir file = do
  isFile <- isRegularFile <$> getSymbolicLinkStatus (dir </> file)
  return $ notDb file && isFile
  where
    notDb = uncurry (&&) . ((/= traDb) &&& (/= traDbTemp))

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
