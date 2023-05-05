{-# LANGUAGE OverloadedStrings #-}

module WebServer.Http.Types where

import Text.Regex.TDFA ( (=~~) )
import Data.ByteString.Char8 (ByteString)
import Data.CaseInsensitive (CI)
import Data.Map (Map)

newtype HttpVersion = HttpVersion ByteString

type RegexResult = (ByteString, ByteString, ByteString, [ByteString])

parseVersion :: ByteString -> Maybe HttpVersion
parseVersion str =
  (str =~~ versionRegex :: Maybe RegexResult) >>= extractVersion
  where
      versionRegex :: ByteString
      versionRegex = "HTTP/([0-9\\.]+)"
      extractVersion :: RegexResult -> Maybe HttpVersion
      extractVersion (_, _, _, [version]) = Just (HttpVersion version)
      extractVersion _ = Nothing

type HeaderName = CI ByteString

type Header = (HeaderName, ByteString)

type Headers = Map HeaderName ByteString

-- Bidirectional headers

connection :: HeaderName
connection = "Connection"

contentLength :: HeaderName
contentLength = "Content-Length"

contentType :: HeaderName
contentType = "Content-Type"

-- Request headers

accept :: HeaderName
accept = "Accept"

acceptCharset :: HeaderName
acceptCharset = "Accept-Charset"

expect :: HeaderName
expect = "Expect"

host :: HeaderName
host = "Host"

origin :: HeaderName
origin = "Origin"

range :: HeaderName
range = "Range"

userAgent :: HeaderName
userAgent = "User-Agent"

-- Response headers

allow :: HeaderName
allow = "Allow"

server :: HeaderName
server = "Server"

setCookie :: HeaderName
setCookie = "Set-Cookie"
