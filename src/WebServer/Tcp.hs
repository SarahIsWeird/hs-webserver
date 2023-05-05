{-# LANGUAGE OverloadedStrings #-}

module WebServer.Tcp (createServer) where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever, void)
import WebServer.Http (processRequest)
import Network.Socket
    ( setCloseOnExecIfNeeded,
      defaultHints,
      getAddrInfo,
      openSocket,
      withSocketsDo,
      setSocketOption,
      gracefulClose,
      accept,
      bind,
      listen,
      close,
      withFdSocket,
      AddrInfo(addrAddress, addrFlags, addrSocketType),
      AddrInfoFlag(AI_PASSIVE),
      HostName,
      ServiceName,
      SocketOption(ReuseAddr),
      SocketType(Stream) )

getHostAddress :: Maybe HostName -> ServiceName -> IO AddrInfo
getHostAddress host port = do
  let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
  head <$> getAddrInfo (Just hints) host (Just port)

createServer :: Maybe HostName -> ServiceName -> IO a
createServer host port = withSocketsDo $ do
  addr <- getHostAddress host port
  bracket (open addr) close listener
  where
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 5
      return sock
    listener sock = forever $ bracketOnError (accept sock) (close . fst) $ \(conn, clientAddr) ->
      void $ forkFinally (processRequest conn clientAddr) (const $ gracefulClose conn 5000)
