{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.CaseInsensitive (mk)
import Control.Lens
import Data.Aeson.Lens
import Network.Wreq
import qualified Data.ByteString as S
import Data.ByteString.Char8 (pack, unpack)
import Text.JSON
import Text.Printf
import Data.Map

type ApiKey = S.ByteString
type UrlPart = S.ByteString
type Organization = UrlPart
type ApiFunction = UrlPart
type Url = String

data Project = Project {}
data Task = Task {title :: String, body :: String}

jsonData d = pack . encode $ toJSObject d

url :: Organization -> ApiFunction -> Url
url org function =
  printf
  "https://%s.kanbanize.com/index.php/api/kanbanize/%s/format/json"
  (unpack org) (unpack function)

apiKey :: Organization -> String -> String -> IO (Maybe a)
apiKey org email password = do
  let postData = (jsonData [("email", email), ("pass", password)])
  resp <- post (url org "login")  postData
  return $ resp ^. responseBody ^? key "apikey"


cperivolApiKey = ("Fwb6F8TNoj6smRz2ECP6XUUr8FfJFq5kyxLy974C" :: ApiKey)

apiKeyHeader :: ApiKey -> Options -> Options
apiKeyHeader ak = header (mk $ pack "apikey") .~ [ak]
