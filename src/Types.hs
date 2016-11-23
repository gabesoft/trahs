{-# LANGUAGE TemplateHaskell #-}

-- | Data types
module Types where

import Control.Lens
import qualified Data.Map as Map

type ReplicaId = String

type VersionNr = Int

type VersionMap = Map.Map ReplicaId VersionNr

data Command
  = FetchState
  | FetchFile FilePath
  | Turn
  deriving (Eq, Show, Read)

data FileMeta = FileMeta
  { _fileReplica :: ReplicaId
  , _fileVersion :: VersionNr
  , _fileSha :: String
  } deriving (Eq, Show)

makeLenses ''FileMeta

data GlobalMeta = GlobalMeta
  { _globalReplica :: ReplicaId
  , _globalVersion :: VersionNr
  , _versionVector :: VersionMap
  } deriving (Eq, Show)

makeLenses ''GlobalMeta