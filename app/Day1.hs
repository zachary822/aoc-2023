module Main where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe

names :: [(String, Char)]
names =
  [ ("one", '1')
  , ("two", '2')
  , ("three", '3')
  , ("four", '4')
  , ("five", '5')
  , ("six", '6')
  , ("seven", '7')
  , ("eight", '8')
  , ("nine", '9')
  ]

getFirstLast :: String -> Integer
getFirstLast [] = error "bad input"
getFirstLast [a] = read [a, a]
getFirstLast xs = read [head xs, last xs]

lookupPrefix :: String -> Maybe Char
lookupPrefix w = if null matches then Nothing else Just ((snd . head) matches)
 where
  matches = filter (\(n, _) -> n `isPrefixOf` w) names

replaceWord :: String -> [Maybe Char]
replaceWord [] = []
replaceWord w@(x : xs) = p : replaceWord xs
 where
  p = lookupPrefix w <|> (if isDigit x then Just x else Nothing)

main :: IO ()
main = do
  xs <- lines <$> readFile "day1.txt"

  print "part 1"
  print $ sum . map (getFirstLast . filter isDigit) $ xs

  print "part 2"
  print $ sum . map (getFirstLast . catMaybes . replaceWord) $ xs

  print "done!"
