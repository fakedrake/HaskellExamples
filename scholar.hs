import System.Environment
import System.Random
import Control.Concurrent
import Data.List
import Text.Regex.Posix
import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Control.Lens
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

type Citations = String

delayed :: IO a -> IO a
delayed = seq getStdRandom (randomR (1,6)) >>= threadDelay
urlPrefix = "https://scholar.google.com/scholar?q="
profilePrefix = "https://scholar.google.com/citations?user="
profilePattern = "citations\\?user=([^&\"]*)"

getPage :: Sess.Session -> String -> IO String
getPage sess url = do
  resp <- Sess.get sess url
  putStrLn $ "Get: " ++ url
  return $ L.unpack $ resp ^. responseBody

getProfileUrl :: Sess.Session -> String -> IO (Maybe String)
getProfileUrl sess name = do
  html <- getPage sess $ urlPrefix ++ name
  let strs = html =~ profilePattern :: [[String]]
  return $ maybe Nothing (Just . (++) profilePrefix) (strs ^? ix 0 . ix 1)

dropUntil :: ([a] -> Bool) -> [a] -> [a]
dropUntil f (x:xs) = if f (x:xs) then x:xs else dropUntil f xs
dropUntil f [] = []

getCitationsHtml :: String -> Citations
getCitationsHtml html =
  (allNums $ subText html) !! 1
  where
    subText = dropUntil $ isPrefixOf ">Citations</a>"
    allNums txt = let (_, x, xs) = txt =~ "[0-9]+" :: (String,String,String) in (x:allNums xs)

getCitationsUrl :: Sess.Session -> String -> IO (Maybe String)
getCitationsUrl sess url = do
  html <- getPage sess url
  let chtml = getCitationsHtml html
  if chtml /= [] then return (Just chtml) else return Nothing

getCitationsName :: Sess.Session -> String -> IO (Maybe Citations)
getCitationsName sess name = do
  purl <- getProfileUrl sess name
  maybe (return Nothing) (getCitationsUrl sess) purl

getCitations :: String -> IO (Maybe Citations)
getCitations name = Sess.withSession $ \s -> getCitationsName s name

getNames :: FilePath -> IO [String]
getNames fp = readFile fp >>= return . lines

namesWithCitations :: FilePath -> IO [(String, Citations)]
namesWithCitations fp = do
  names <- getNames fp
  citations <- map (delayed . getCitations) names
  return $ catMaybes $ zipWith myZipper names citations
  where
    myZipper :: String -> IO (Maybe Citations) -> IO (Maybe (String, Citations))
    myZipper n c = if isJust c then Just (n, fromJust c) else Nothing


processNamesFile fname = namesWithCitations fname >>= processNames

processNames :: [(Citations, String)] -> IO ()
processNames [] = return ()
processNames ((n,c):xs) = do
  forkIO (putStrLn $ n ++ ": " ++ c)
  processNames xs

main = do
  args <- getArgs
  let msg = "Single argument is the filename. args: " ++ (show $ args)
  if length args < 1
    then putStrLn msg
    else processNamesFile $ head args
