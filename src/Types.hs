{-# LANGUAGE TemplateHaskell #-}

-- | Data types
module Types where

import Control.Lens
import qualified Data.Map as Map

type ReplicaId = String

type VersionNr = Int

type VersionMap = Map.Map ReplicaId VersionNr

type FileMetaMap = Map.Map FilePath FileMeta

data Command
  = FetchMeta
  | FetchFile FilePath
  | InitSync
  | Turn
  | Done
  deriving (Eq, Show, Read)

data FileMeta = FileMeta
  { _fileReplica :: ReplicaId
  , _fileVersion :: VersionNr
  , _fileSha :: String
  } deriving (Eq, Show, Read)

makeLenses ''FileMeta

data GlobalMeta = GlobalMeta
  { _globalVersion :: VersionNr
  , _versionVector :: VersionMap
  , _fileMetaMap :: FileMetaMap
  , _globalReplica :: ReplicaId
  } deriving (Eq, Show, Read)

makeLenses ''GlobalMeta
