{-# LANGUAGE OverloadedStrings #-}

module WebServer.Http.Response (Response (..), responseToBuilder) where

import Data.ByteString.Builder (Builder, byteString, intDec)
import Data.ByteString.Char8 (ByteString)
import Data.CaseInsensitive (CI (original))
import WebServer.Http.StatusCodes
    ( StatusCode(sDescription, sValue) )
import WebServer.Http.Types (Header, Headers)
import qualified Data.Map as Map

data Response = Response
  { status :: StatusCode,
    headers :: Headers,
    content :: ByteString
  }

makeRequestLine :: Response -> Builder
makeRequestLine response =
  mconcat
    [ "HTTP/1.1 ",
      intDec (sValue statusCode),
      " ",
      byteString (sDescription statusCode),
      "\r\n"
    ]
  where
    statusCode = status response

makeHeaders :: Response -> [Builder]
makeHeaders response =
  map makeHeader (Map.toAscList $ headers response)
  where
    makeHeader :: Header -> Builder
    makeHeader (name, value) = byteString (original name) <> ": " <> byteString value <> "\r\n"

responseToBuilder :: Response -> Builder
responseToBuilder response =
  makeRequestLine response <> mconcat (makeHeaders response) <> "\r\n" <> byteString (content response)
