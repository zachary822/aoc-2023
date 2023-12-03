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

getFirstLast :: String -> String
getFirstLast [] = error "bad input"
getFirstLast [a] = [a, a]
getFirstLast xs = [head xs, last xs]

lookupPrefix :: String -> Maybe Char
lookupPrefix w = snd <$> find ((`isPrefixOf` w) . fst) names

replaceWord :: String -> [Maybe Char]
replaceWord [] = []
replaceWord w@(x : xs) = p : replaceWord xs
 where
  p = lookupPrefix w <|> (if isDigit x then Just x else Nothing)

main :: IO ()
main = do
  xs <- lines <$> readFile "day1.txt"

  print "part 1"
  print (sum . map (read . getFirstLast . filter isDigit) $ xs :: Integer)

  print "part 2"
  print (sum . map (read . getFirstLast . catMaybes . replaceWord) $ xs :: Integer)

  print "done!"
