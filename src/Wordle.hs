module Wordle where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client.TLS

main :: IO ()
main = do
  wordsSys <- Set.fromList <$> wordsFromSystemDictionary
  wordsGH <- Set.fromList <$> wordsFromGitHubList
  let (left, right) = (wordsSys \\ wordsGH, wordsGH \\ wordsSys)
  putStrLn "\n\nLeft\n>>>>"
  Foldable.traverse_ putStrLn left
  putStrLn "\n\nRight\n>>>>>"
  Foldable.traverse_ putStrLn right

-- Foldable.traverse_ putStrLn words'
-- print (length words')

wordsFromSystemDictionary :: IO [String]
wordsFromSystemDictionary = do
  words' <- words <$> readFile "/usr/share/dict/words"
  pure (filter ((&&) <$> (==) 5 . length <*> all Char.isLower) words')

wordsFromGitHubList :: IO [String]
wordsFromGitHubList = do
  manager <- Client.newManager Client.TLS.tlsManagerSettings
  let url = "https://raw.githubusercontent.com/tabatkins/wordle-list/main/words"
  request <- Client.parseRequest url
  response <- Client.httpLbs request manager
  pure (words (Char8.unpack (Client.responseBody response)))
-- main :: IO ()
-- main = do
--   manager <- newManager defaultManagerSettings

--   request <- parseRequest "http://httpbin.org/get"
--   response <- httpLbs request manager

--   putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
--   print $ responseBody response

-- mapM_ putStrLn fiveLetterWords
-- print (length fiveLetterWords)
