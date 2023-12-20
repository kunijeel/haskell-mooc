module Examples.PathServer where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status (Status, status200, status404)
import Network.Wai (Application, Response, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)
import Database.SQLite.Simple

createQuery :: Query
createQuery = Query (T.pack "CREATE TABLE IF NOT EXISTS phonebook (name TEXT, phone TEXT);")

addQuery :: Query
addQuery = Query (T.pack "INSERT INTO phonebook (name, phone) VALUES (?, ?);")

addToPhonebook :: Connection -> String -> String -> IO ()
addToPhonebook db name phone = execute db addQuery (name, phone)

getQuery :: Query
getQuery = Query (T.pack "SELECT phone FROM phonebook WHERE name = ?;")

getNumbersFor :: Connection -> String -> IO [String]
getNumbersFor db name = do
  rows <- query db getQuery (Only name)
  return $ map fromOnly rows

openDatabase :: IO Connection
openDatabase = do
  db <- open "phonebook.db"
  execute_ db createQuery
  return db

-- helper for constructing Responses
makeResponse :: Status -> T.Text -> Response
makeResponse status text =
  responseLBS status [] (B.fromStrict (encodeUtf8 text))

serveHelloWorld :: IO Response
serveHelloWorld =
  return $ makeResponse status200 (T.pack "Hello World!")

addToPhonebookHandler :: Connection -> T.Text -> T.Text -> IO Response
addToPhonebookHandler db name phone = do
  addToPhonebook db (T.unpack name) (T.unpack phone)
  return $ makeResponse status200 (T.pack "Successfully added!")

queryPhonebookHandler :: Connection -> T.Text -> IO Response
queryPhonebookHandler db name = do
  numbers <- getNumbersFor db (T.unpack name)
  let responseText = T.pack $ show (length numbers) ++ " numbers\n" ++ unlines numbers
  return $ makeResponse status200 responseText

serveSource :: IO Response
serveSource = do
  source <- readFile "PathServer.hs"
  return $ makeResponse status200 (T.pack source)

serveSecret :: IO Response
serveSecret =
  return $ makeResponse status200 (T.pack "the secret is swordfish")

serveNotFound :: [T.Text] -> IO Response
serveNotFound path =
  let showPath = T.intercalate (T.pack "/") path
      contents = T.append (T.pack "Not found: ") showPath
  in return $ makeResponse status404 contents

-- we can't pattern match on Text, so we use guards and (==)
servePath :: Connection -> [T.Text] -> IO Response
servePath db path
  | null path = serveHelloWorld  -- 루트 경로("/")에서 "Hello World!"를 반환
  | length path == 3 && head path == T.pack "add" = addToPhonebookHandler db (path !! 1) (path !! 2)
  | length path == 2 && head path == T.pack "query" = queryPhonebookHandler db (path !! 1)
  | path == [T.pack "source"] = serveSource
  | path == [T.pack "secret", T.pack "file"] = serveSecret
  | otherwise = serveNotFound path

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application :: Connection -> Application
application db request respond = do
  let path = pathInfo request
  response <- servePath db path
  respond response

port :: Int
port = 3421

main :: IO ()
main = do
  db <- openDatabase
  run port (application db)
  close db