module Main where

import qualified WebServer

main :: IO ()
main = WebServer.createServer Nothing "8080"
