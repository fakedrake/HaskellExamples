-- This will lazily read one by one all the _leaf_ nodes of an xml
-- file.
--
-- λ> :set +t
-- λ> main
-- ("a","hi0")
-- ("a","hi1")
-- ("a","hi2")
-- it :: ()
-- (0.01 secs, 3,219,688 bytes)
--
-- Which is pretty fast for a 3G file

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as U
import Prelude hiding (readFile)
import Data.Maybe
import Text.XML.Expat.SAX
import System.Environment (getArgs)
import Data.List

main :: IO ()
main = do
  -- Generate a huge test.xml
  -- (echo "<head>"; for i in {0..1000000}; do echo "<a>hi$i</a>"; done ; echo "</head>") > /tmp/test.xml
  contents <- BSL.readFile "/tmp/test.xml"
  let events = parse defaultParseOptions contents
  mapM_ print $ take 1000 $ mapMaybe toPair $ tags events

data MyTag = Unclosed String String | Tag String String | NoTag deriving Show
data Page = Page {text :: String, title :: String} deriving Show

-- Turn an event stream into a tag strem
tags :: [(SAXEvent String String)] -> [MyTag]
tags [] = []
tags (x:xs) = (foldl' buildLeaf NoTag h):(tags t) where
  cns = break isStart xs
  h = x:fst cns
  t = snd cns

-- True if a tag is starting
isStart :: SAXEvent String String -> Bool
isStart (StartElement _ _) = True
isStart _ = False

toPair :: MyTag -> Maybe (String, String)
toPair (Tag n t) = Just (n, t)
toPair _ = Nothing

-- Compose tags from events
buildLeaf :: MyTag -> SAXEvent String String -> MyTag
buildLeaf NoTag (StartElement name _) = Unclosed name ""
buildLeaf NoTag _ = NoTag
buildLeaf (Tag tn tt) _ = Tag tn tt
buildLeaf (Unclosed tn tt) (StartElement _ _) = Tag tn tt
buildLeaf (Unclosed tn tt) (EndElement tname)
  | tname == tn = (Tag tn tt)
  | otherwise = (Unclosed tn tt)
buildLeaf (Unclosed tn tt) (CharacterData txt) = (Unclosed tn (tt++txt))
buildLeaf t _ = t

toSql :: MyTag -> SqlTag
toSql =
