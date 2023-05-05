{-# LANGUAGE OverloadedStrings #-}

module WebServer.Http where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Network.Socket.ByteString (recv, sendAll)
import WebServer.Http.Response ( responseToBuilder, Response(..) )
import qualified WebServer.Http.StatusCodes as SC
import qualified WebServer.Http.Types as T
import WebServer.Http.Request (parseRequest, Request)
import qualified WebServer.Http.Request as Req
import qualified Data.Map as Map
import Data.Map ((!?))
import Network.Socket (Socket, SockAddr, close)

defaultServerSignature :: ByteString
defaultServerSignature = "PastelServer/0.0.1"

getResponse :: Request -> IO Response
getResponse req =
  pure $ Response SC.ok (Map.fromList [(T.contentType, "text/html")]) b
  where
    b = "<html><head><title>Hi :D</title><body><h1>Hello " <> name <> "! uwu</h1></body></html>"
    name = Map.findWithDefault "someone" T.host (Req.headers req)

receiveAll :: Socket -> IO ByteString
receiveAll sock = receiveAll' ""
  where
    receiveAll' :: ByteString -> IO ByteString
    receiveAll' acc = do
      nextPart <- recv sock 1024
      let newAcc = B.append acc nextPart
      if B.length nextPart == 1024
        then receiveAll' newAcc
        else pure newAcc

getBadRequestResponse :: Response
getBadRequestResponse =
  Response SC.badRequest Map.empty "Malformed HTTP request"

postProcessResponse :: Response -> Response
postProcessResponse res =
  Response (status res) (Map.union (headers res) (Map.fromList $ common <> additional)) (content res)
  where
    common = [(T.connection, "close"), (T.server, defaultServerSignature)] :: [T.Header]
    additional = case status res of
      SC.StatusCode 200 _ -> extendedHeadersForOk
      _ -> []
    extendedHeadersForOk =
      let ct = case headers res !? T.contentType of
            Nothing -> [(T.contentType, "text/plain; charset=utf-8")]
            Just _ -> []
          cl = case headers res !? T.contentLength of
            Nothing -> [(T.contentLength, B.pack . show . B.length $ content res)]
            Just _ -> []
        in ct <> cl

processRequest :: Socket -> SockAddr -> IO ()
processRequest sock addr = do
  putStrLn $ "Incoming request from " <> show addr
  requestStr <- receiveAll sock
  res <- case parseRequest requestStr of
    Nothing -> pure getBadRequestResponse
    Just req -> getResponse req
  let ppRes = postProcessResponse res
      resByteString = B.toStrict . toLazyByteString . responseToBuilder $ ppRes
  sendAll sock resByteString
  close sock
  pure ()