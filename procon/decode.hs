{-# LANGUAGE LambdaCase, OverloadedStrings #-}
import Encoding
import OldEncoding
import Data.Char
import Control.Lens
import System.Environment
import Control.Monad
import Data.String
import Network.HTTP.Types
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Data.Monoid
import Control.Monad.IO.Class
import Text.Printf
import Control.Exception
import Control.Concurrent
import Network (withSocketsDo)
import Network.HTTP.Types

token :: B.ByteString
token = "3232599388"

url :: String
url = "http://192.168.2.2/SubmitAnswer"

sendContent xs = withSocketsDo $ do
    let p = transcode # map digitToInt (concat $ lines $ xs)
    putStrLn p
    req <- parseUrl url
    let qs = renderSimpleQuery False [("playerid", token), ("answer", fromString p)]
    B.putStrLn qs
    let req' = req {method = "POST", requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")], requestBody = RequestBodyBS qs}
    withManager $ \manager -> httpLbs req' manager >>= liftIO . BL.putStrLn . responseBody

tryFetch :: String -> IO B.ByteString
tryFetch url = do
    try (simpleHttp url) >>= \case
        Left e -> print (e :: HttpException) >> threadDelay (2 * 1000 * 1000) >> tryFetch url
        Right a -> return (a ^. strict)

main = getArgs >>= \case
    ("olddecode":_) ->
        forever $ do
            xs <- getLine
            putStrLn $ oldTranscode # map digitToInt xs
    ("decode":path:_) -> do
        xs <- readFile path
        putStrLn $ transcode # map digitToInt (concat $ lines $ xs)
    ("decode":_) -> do
        forever $ do
            xs <- getLine
            putStrLn $ transcode # map digitToInt xs
    ("fetch":_) -> withSocketsDo $ forM_ [(1::Int)..] $ \i -> do
        let ident = "11"
        let name = ident ++ "_" ++ printf "%03d" i ++ ".jpg"
        let url = "http://192.168.2.2/image/" ++ name
        putStrLn url
        tryFetch url >>= B.writeFile name
        threadDelay $ 2 * 1000 * 1000
    ("send":path:_) -> do
        xs <- readFile path
        sendContent xs
    ("send":_) -> do
        xs <- getLine
        sendContent xs