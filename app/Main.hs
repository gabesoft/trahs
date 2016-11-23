module Main where

import Codec.Digest.SHA
import Control.Applicative
import Control.Lens
import Data.ByteString (hPut, hGet)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Prelude hiding (log)
import System.Environment
import System.Exit
import System.IO
import System.Process
import Trahs
import Types

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--server", l] -> do
      hSetBuffering stdout LineBuffering
      server stdin stdout l
    [r, l]
      | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do
      hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
      exitFailure

-- |
-- Command for executing trahs on a remote system. The '@' will be replaced by
-- the hostname, and the directory will be appended.
trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"

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
mkGlobalMeta = GlobalMeta 1 (Map.empty) <$> mkReplicaId

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

-- |
-- @server r w dir@ runs the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@.
server :: Handle -> Handle -> FilePath -> IO ()
server r w dir
  -- hPutStrLn w "I am the server"
  -- log "Server ready, waiting for command ..."
  -- TODO use bytestring hPut & hGet
 = do
  cmd <- readCmd r
  case cmd of
    Done -> return ()
    Turn -> client False r w dir
    _ -> do
      hPutStrLn w ("Command received " ++ show cmd)
      server r w dir

client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir
  -- line1 <- hGetLine r
  -- log ("The server said1 " ++ show line1)
 = do
  sendCmd w FetchState
  line2 <- hGetLine r
  log ("The server said " ++ show line2)
  sendCmd w (FetchFile "/etc/passwd")
  line3 <- hGetLine r
  log ("The server said " ++ show line3)
  if turn
    then sendCmd w Turn >> server r w dir
    else sendCmd w Done

log :: String -> IO ()
log = hPutStrLn stderr

sendCmd :: Handle -> Command -> IO ()
sendCmd h = hPutStrLn h . show

readCmd :: Handle -> IO Command
readCmd h = read <$> hGetLine h

hostCmd :: String -> FilePath -> IO String
hostCmd host dir = do
  tmpl <- maybe trassh id <$> lookupEnv "TRASSH"
  case break (== '@') tmpl of
    (b, '@':e) -> return $ b ++ host ++ e ++ ' ' : dir
    _ -> return $ tmpl ++ ' ' : dir

spawnRemote :: String -> FilePath -> IO (Handle, Handle)
spawnRemote host dir = do
  cmd <- hostCmd host dir
  hPutStrLn stderr ("running " ++ show cmd)
  (Just w, Just r, _, _) <-
    createProcess (shell cmd) {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering w LineBuffering
  return (r, w)

connect :: String -> FilePath -> FilePath -> IO ()
connect host rdir ldir = do
  (r, w) <- spawnRemote host rdir
  client True r w ldir
