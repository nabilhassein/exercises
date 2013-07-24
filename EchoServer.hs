module TrackerServer where

import Control.Concurrent (forkIO)
import Control.Monad      (forever)
import Network            (PortID(..), listenOn, accept, Socket)
import System.IO          (hSetBuffering, Handle, BufferMode(NoBuffering))
import qualified Data.ByteString.Lazy as BL


port :: PortID
port  = PortNumber 7070

main :: IO ()
main  = listenOn port >>= handler

handler :: Socket -> IO ()
handler    s       = forever $ do
  (h,_,_) <- accept s 
  hSetBuffering h NoBuffering
  forkIO $ echo h

echo :: Handle -> IO ()
echo    handle = forever $ BL.hGetContents handle >>= BL.hPutStr handle
