module Main where

import Control.Applicative
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
-- @server r w dir@ runs the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@.
server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  hPutStrLn w "I am the server"
  cmd1 <- readCmd r
  hPutStrLn w ("Command1 received " ++ show cmd1)
  cmd2 <- readCmd r
  hPutStrLn w ("Command2 received " ++ show cmd2)

client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
  line1 <- hGetLine r
  log ("The server said " ++ show line1)
  sendCmd w FetchState
  sendCmd w (FetchFile "/etc/passwd")
  line2 <- hGetLine r
  log ("The server said " ++ show line2)
  line3 <- hGetLine r
  log ("The server said " ++ show line3)
  -- At the end, if turn == True, then we issue some command to swap
  -- roles and run server r w dir.
  return ()

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
