module Main where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad (guard, filterM)
import Data.ByteString (hPut, hGet)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Prelude hiding (log)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Trahs
import Types
import Sync

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
      hPutStrLn w ("Command received " ++ dir ++ " " ++ show cmd)
      server r w dir

client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir
  -- line1 <- hGetLine r
  -- log ("The server said1 " ++ show line1)
 = do
  log ("This is the client " ++ dir)
  sendCmd w FetchMeta
  line2 <- hGetLine r
  log ("The server said " ++ show line2)
  sendCmd w (FetchFile "/etc/passwd")
  line3 <- hGetLine r
  log ("The server said " ++ show line3)
  if turn
    then sendCmd w Turn >> server r w dir
    else sendCmd w Done

-- |
-- Log a message to stderr
log :: String -> IO ()
log = hPutStrLn stderr

-- |
-- Send a command to the process at @handle@
sendCmd :: Handle -> Command -> IO ()
sendCmd handle = hPutStrLn handle . show

-- |
-- Read a command from the process at @handle@
readCmd :: Handle -> IO Command
readCmd handle = read <$> hGetLine handle

-- |
-- Create a command used to connect to a remote @host@ with @dir@
hostCmd :: String -> FilePath -> IO String
hostCmd host dir = do
  tmpl <- maybe trassh id <$> lookupEnv "TRASSH"
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
