module Main where

import Control.Applicative
import Control.Monad (forM_)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromMaybe)
import Meta
import Prelude hiding (log)
import Sync
import System.Directory (removeFile, renameFile)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
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
-- Run the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@
server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  cmd <- readCmd r
  case cmd of
    InitSync -> initSyncL dir >> server r w dir
    FetchMeta -> readGlobalMeta dir >>= sendAsBytes w >> server r w dir
    FetchFile path -> sendFile w (dir </> path) >> server r w dir
    Done -> return ()
    Turn -> client False r w dir

-- |
-- Run the sync algorithm from server to client in @dir@,
-- reading input from @r@ and writing it to @w@
client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
  sendCmd w InitSync
  initSyncL dir
  sendCmd w FetchMeta
  serverMeta <- readAsBytes r
  localMeta <- readGlobalMeta dir
  let (meta, actions) = mergeMeta localMeta serverMeta
  forM_ actions (executeActionL r w dir)
  writeGlobalMeta dir meta
  if turn
    then sendCmd w Turn >> server r w dir
    else sendCmd w Done

initSyncL :: FilePath -> IO ()
initSyncL dir = log ("Init sync in " ++ dir) >> initSync dir

-- |
-- Execute a @SyncAction@ and print it to the log
executeActionL :: Handle -> Handle -> FilePath -> SyncAction -> IO ()
executeActionL r w dir cmd = log (show cmd) >> executeAction r w dir cmd

-- |
-- Execute a @SyncAction@
executeAction :: Handle -> Handle -> FilePath -> SyncAction -> IO ()
executeAction _ _ _ Noop = return ()
executeAction _ _ dir (DeleteFile path) = removeFile (dir </> path)
executeAction r w dir (DownloadFile path) = do
  sendCmd w (FetchFile path)
  fetchFile r >>= B.writeFile (dir </> path)
executeAction r w dir (FlagConflict path pLocal pRemote) = do
  sendCmd w (FetchFile path)
  fetchFile r >>= B.writeFile (dir </> pRemote)
  renameFile (dir </> path) (dir </> pLocal)

-- |
-- Send the file at @path@ to the process referenced by @handle@
sendFile :: Handle -> FilePath -> IO ()
sendFile handle path = do
  bytes <- B.readFile path
  hPrint handle (B.length bytes)
  B.hPut handle bytes

-- |
-- Fetch a file from the process referenced by @handle@
fetchFile :: Handle -> IO B.ByteString
fetchFile handle = do
  count <- read <$> hGetLine handle
  B.hGet handle count

-- |
-- Send an object as a @ByteString@ to the process referenced by @handle@
sendAsBytes
  :: Show a
  => Handle -> a -> IO ()
sendAsBytes handle cargo = do
  let bytes = pack (show cargo)
  hPrint handle (B.length bytes)
  B.hPut handle bytes

-- |
-- Read an object as a @ByteString@ from the process represented by @handle@
readAsBytes
  :: Read a
  => Handle -> IO a
readAsBytes handle = do
  count <- read <$> hGetLine handle
  bytes <- B.hGet handle count
  return (read $ unpack bytes)

-- |
-- Log a message to stderr
log :: String -> IO ()
log = hPutStrLn stderr

-- |
-- Send a command to the process at @handle@
sendCmd :: Handle -> Command -> IO ()
sendCmd = hPrint

-- |
-- Read a command from the process at @handle@
readCmd :: Handle -> IO Command
readCmd handle = read <$> hGetLine handle

-- |
-- Create a command used to connect to a remote @host@ with @dir@
hostCmd :: String -> FilePath -> IO String
hostCmd host dir = do
  tmpl <- fromMaybe trassh <$> lookupEnv "TRASSH"
  case break (== '@') tmpl of
    (b, '@':e) -> return $ b ++ host ++ e ++ ' ' : dir
    _ -> return $ tmpl ++ ' ' : dir

-- |
-- Spawn a remote process on @host@ with @dir@
spawnRemote :: String -> FilePath -> IO (Handle, Handle)
spawnRemote host dir = do
  cmd <- hostCmd host dir
  hPutStrLn stderr ("running " ++ show cmd)
  (Just w, Just r, _, _) <-
    createProcess (shell cmd) {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering w LineBuffering
  return (r, w)

-- |
-- Connect the remote host process and the local client
connect :: String -> FilePath -> FilePath -> IO ()
connect host rdir ldir = do
  (r, w) <- spawnRemote host rdir
  client True r w ldir
