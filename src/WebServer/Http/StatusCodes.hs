{-# LANGUAGE OverloadedStrings #-}

module WebServer.Http.StatusCodes where

import Data.ByteString.Char8 (ByteString)

data StatusCode = StatusCode
  { sValue :: Int,
    sDescription :: ByteString
  }

-- Success status codes
ok :: StatusCode
ok = StatusCode 200 "OK"
created :: StatusCode
created = StatusCode 201 "Created"
accepted :: StatusCode
accepted = StatusCode 202 "Accepted"
nonAuthoritative :: StatusCode
nonAuthoritative = StatusCode 203 "Non-Authoritative Information"
noContent :: StatusCode
noContent = StatusCode 204 "No Content"
resetContent :: StatusCode
resetContent = StatusCode 205 "Reset Content"
partialContent :: StatusCode
partialContent = StatusCode 206 "Partial Content"
-- 207 Multi-Status and 208 Already Reported are WebDAV status codes, which isn't supported.

-- Redirection status codes
multipleChoices :: StatusCode
multipleChoices = StatusCode 300 "Multiple Choices"
movedPermanently :: StatusCode
movedPermanently = StatusCode 301 "Moved Permanently"
-- 302 Found is deprecated and either 301 Moved Permanently, 303 See Other, 307 Temporary Redirect or 308 Permanent Redirect should be used.
seeOther :: StatusCode
seeOther = StatusCode 303 "See Other"
notModified :: StatusCode
notModified = StatusCode 304 "Not Modified"
-- 305 Use Proxy is ignored by IE and Firefox, and only seldomly used.
-- 306 Switch Proxy is no longer used.
temporaryRedirect :: StatusCode
temporaryRedirect = StatusCode 307 "Temporary Redirect"
permanentRedirect :: StatusCode
permanentRedirect = StatusCode 308 "Permanent Redirect"

-- Client error status codes
badRequest :: StatusCode
badRequest = StatusCode 400 "Bad Request"
unauthorized :: StatusCode
unauthorized = StatusCode 401 "Unauthorized"
-- 402 Payment Required is reserved for future use.
forbidden :: StatusCode
forbidden = StatusCode 403 "Forbidden"
notFound :: StatusCode
notFound = StatusCode 404 "Not Found"
methodNotAllowed :: StatusCode
methodNotAllowed = StatusCode 405 "Method Not Allowed"
notAcceptable :: StatusCode
notAcceptable = StatusCode 406 "Not Acceptable"
proxyAuthenticationRequired :: StatusCode
proxyAuthenticationRequired = StatusCode 407 "Proxy Authentication Required"
requestTimeout :: StatusCode
requestTimeout = StatusCode 408 "Request Timeout"
conflict :: StatusCode
conflict = StatusCode 409 "Conflict"
gone :: StatusCode
gone = StatusCode 410 "Gone"
lengthRequired :: StatusCode
lengthRequired = StatusCode 411 "Length Required"
preconditionFailed :: StatusCode
preconditionFailed = StatusCode 412 "Precondition Failed"
payloadTooLarge :: StatusCode
payloadTooLarge = StatusCode 413 "Payload Too Large"
uriTooLong :: StatusCode
uriTooLong = StatusCode 414 "URI Too Long"
unsupportedMediaType :: StatusCode
unsupportedMediaType = StatusCode 415 "Unsupported Media Type"
rangeNotSatisfiable :: StatusCode
rangeNotSatisfiable = StatusCode 416 "Range Not Satisfiable"
expectationFailed :: StatusCode
expectationFailed = StatusCode 417 "Expectation Failed"
imATeapot :: StatusCode
imATeapot = StatusCode 418 "I'm a teapot"
misdirectedRequest :: StatusCode
misdirectedRequest = StatusCode 421 "Misdirected Request"
unprocessableEntity :: StatusCode
unprocessableEntity = StatusCode 422 "Unprocessable Entity"
-- 423 Locked and 424 Failed Dependency are WebDAV status codes, which isn't supported.
tooEarly :: StatusCode
tooEarly = StatusCode 425 "Too Early"
-- 426 Upgrade Required tells the client to use SSL, which isn't supported.
preconditionRequired :: StatusCode
preconditionRequired = StatusCode 428 "Precondition Required"
tooManyRequests :: StatusCode
tooManyRequests = StatusCode 429 "Too Many Requests"
requestHeaderFieldsTooLarge :: StatusCode
requestHeaderFieldsTooLarge = StatusCode 431 "Request Header Fields Too Large"
unavailableForLegalReasons :: StatusCode
unavailableForLegalReasons = StatusCode 451 "Unavailable For Legal Reasons"

-- Server error status codes
internalServerError :: StatusCode
internalServerError = StatusCode 500 "Internal Server Error"
notImplemented :: StatusCode
notImplemented = StatusCode 501 "Not implemented"
badGateway :: StatusCode
badGateway = StatusCode 502 "Bad gateway"
serviceUnavailable :: StatusCode
serviceUnavailable = StatusCode 503 "Service Unavailable"
gatewayTimeout :: StatusCode
gatewayTimeout = StatusCode 504 "Gateway Timeout"
httpVersionNotSupported :: StatusCode
httpVersionNotSupported = StatusCode 505 "HTTP Version Not Supported"
variantAlsoNegotiates :: StatusCode
variantAlsoNegotiates = StatusCode 506 "Variant Also Negotiates"
-- 507 Insufficient Storage and 508 Loop Detected are WebDAV status codes, which isn't supported.
notExtended :: StatusCode
notExtended = StatusCode 510 "Not Extended"