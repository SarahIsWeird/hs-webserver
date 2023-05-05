{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module WebServer.Http.Request where

import Text.Regex.TDFA ( (=~), (=~~) )
import Data.ByteString.Char8 (ByteString, breakSubstring)
import qualified Data.ByteString as B
import WebServer.Http.Types ( Headers, HttpVersion, parseVersion, Header )
import Data.CaseInsensitive (mk)
import qualified Data.Map as Map

data RequestMethod
  = Get
  | Post
  | Put
  | Delete
  | Head
  | Options
  | Patch
  deriving (Eq)

instance Show RequestMethod where
  show :: RequestMethod -> String
  show Get = "GET"
  show Post = "POST"
  show Put = "PUT"
  show Delete = "DELETE"
  show Head = "HEAD"
  show Options = "OPTIONS"
  show Patch = "PATCH"

parseMethod :: ByteString -> Maybe RequestMethod
parseMethod method =
  case method of
    "GET" -> Just Get
    "POST" -> Just Post
    "PUT" -> Just Put
    "DELETE" -> Just Delete
    "HEAD" -> Just Head
    "OPTIONS" -> Just Options
    "PATCH" -> Just Patch
    _ -> Nothing

data Request = Request
  { requestMethod :: RequestMethod,
    path :: ByteString,
    headers :: Headers,
    body :: Maybe ByteString
  }
  deriving (Show)

data RequestHeader =
  RequestHeader RequestMethod ByteString HttpVersion

parseRequestHeader :: ByteString -> Maybe RequestHeader
parseRequestHeader line =
  if line =~ isHeaderRegex :: Bool
    then
      let (method, _, rest) = line =~ (" " :: ByteString) :: (ByteString, ByteString, ByteString)
          (queryPath, _, version) = rest =~ (" " :: ByteString) :: (ByteString, ByteString, ByteString)
        in RequestHeader <$> parseMethod method <*> Just queryPath <*> parseVersion version
    else Nothing
    where
      isHeaderRegex :: ByteString
      isHeaderRegex = "[A-Z]+ [^ ]+ HTTP/[0-9\\.]+"

parseHeader :: ByteString -> Maybe Header
parseHeader str =
  fmap (\(name, _, value) -> (mk name, value)) match
  where
    match = str =~~ (": " :: ByteString) :: Maybe (ByteString, ByteString, ByteString)

parseHeadersAndBody :: ByteString -> Maybe (Headers, Maybe ByteString)
parseHeadersAndBody s =
  convertHeaderListToMap <$> parseHeadersAndBody' s []
  where
    newline :: ByteString
    newline = "\r\n"
    parseHeadersAndBody' str h =
      let (headerLine, sep, rest) = str =~ newline :: (ByteString, ByteString, ByteString)
        in if sep == ""
          then Nothing
          else
            if headerLine == ""
              then if rest == ""
                then Just (h, Nothing)
                else Just (h, Just rest)
              else
                case parseHeader headerLine of
                  Nothing -> Nothing
                  Just hdr -> parseHeadersAndBody' rest (hdr : h)
    convertHeaderListToMap (hdrs, bdy) = (Map.fromList hdrs, bdy)

parseRequest :: ByteString -> Maybe Request
parseRequest str =
  case breakSubstring "\r\n" str of
    (_, "") -> Nothing
    (reqHeaderStr, rest) ->
      case parseRequestHeader reqHeaderStr of
        Nothing -> Nothing
        Just (RequestHeader method queryPath _) ->
          case parseHeadersAndBody (B.drop 2 rest) of
            Nothing -> Nothing
            Just (hdrs, bdy) -> Just $ Request method queryPath hdrs bdy