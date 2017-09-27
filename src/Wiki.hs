module Wiki
( Path
, getRelated
) where

import Control.Lens 
import Data.Char
import Data.List ((\\), break)
import Network.Wreq 
import Text.Regex.Posix

type Path = (String, String)

type Topic = String
data WikiPage = WikiPage Topic [WikiPage] deriving (Show)
data WikiCrumb = WikiCrumb Topic [WikiPage] [WikiPage] deriving (Show)
type WikiZipper = (WikiPage, [WikiCrumb])

toLowerStr :: String -> String
toLowerStr str = [ toLower chr | chr <- str ]

getRelated :: String -> IO [String]
getRelated topic = do
  putStrLn $ '/':topic
  resp <- get ("https://en.wikipedia.org/wiki/" ++ topic)
  return $ related $ clean $ body resp
  where body = show . (^. responseBody)
        clean = filter (not . unwanted)
        unwanted c = isControl c || c == '\\' || isSpace c

related :: String -> [String]
related html =
  map (extractTopic . toLowerStr) linkUrls
  where linkUrls = getAllTextMatches $ html =~ "\"/wiki/[^\":]+\"" :: [String]
        extractTopic link = filter (/= '"') link \\ "/wiki/"

buildTree :: ([Topic] -> Bool) -> Topic -> IO WikiPage
buildTree f topic = do
  related' <- getRelated topic
  mapM_ print related'
  related'' <- sequence (if f related' then [] else map (buildTree f) related')
  return $ WikiPage topic related''

makeZipper :: Topic -> Topic -> IO WikiZipper
makeZipper start end
  | start == end = return (WikiPage start [], [])
  | otherwise    = do 
    wiki <- buildTree (elem end) start
    return (wiki, [])

--degrees :: Topic -> Topic -> IO WikiZipper
--degrees start end = do 
--  allRelated <- related' start
--  return (WikiPage start allRelated, [])
--  where loadRelated topic = do 
--          html <- wikiHtml topic
--          return $ related html
--        mapOrEmpty topics = 
--          if end `elem` topics then [] else map related' topics
--        related' topic = do 
--          topicRelated <- loadRelated topic
--          topicRelated' <- mapOrEmpty topicRelated
--          return [WikiPage topic topicRelated']

--example :: WikiPage
--example =
--  WikiPage "murder"
--    [ WikiPage "human"
--      [ WikiPage "homo_sapiens" []
--      , WikiPage "murder" []
--      ]
--    , WikiPage "justification"
--      [ WikiPage "defense" 
--        [ WikiPage "criminal_defenses" 
--          [ WikiPage "murder" 
--          ] 
--        ]
--      ]
--    ]

--wikiBack :: WikiZipper -> WikiZipper
--wikiBack (wiki, WikiCrumb topic ls rs:bs) = (WikiPage topic (ls ++ [wiki] ++ rs), bs)

--wikiGoTo :: Topic -> WikiZipper -> WikiZipper
--wikiGoTo topic (WikiPage title related, bs) =
--  let (ls, wiki:rs) = break (topicIs topic) wiki
--  in (wiki, WikiCrumb title ls rs:bs)

--topicIs :: Topic -> WikiPage -> Bool
--topicIs topic (WikiPage title _) = topic == title
