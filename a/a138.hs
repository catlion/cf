import Data.List
import Control.Monad

-- k-th vowel from the end of string
vowelk :: String -> Int -> Maybe Char
vowelk str k =
  if len > pos then Just (f !! (reverse pos))
  else Nothing
    where f = filter (\c -> elem c v) str
          v = ['a','e','i','o','u']
          pos = k + 1
          len = length f

-- Rhyme in part
quadrhyme :: (Char, Char, Char, Char) -> Maybe String
quadrhyme (a, b, c, d) =
  case length x of 1 -> Just "aaaa"
                   2 -> case (a == b, a == c, a == d) of (True, _, _) -> Just "aabb"
                                                         (_, True, _) -> Just "abab"
                                                         (_, _, True) -> Just "abba"
                   _ -> Nothing
    where x = group $ sort [a, b, c, d]

-- Split to parts
quadriate :: [Maybe Char] -> [(Char, Char, Char, Char)]
quadriate [] = []
quadriate [a:b:c:d:tl] = if a == Nothing || b == Nothing || c == Nothing || d == Nothing then quadriate tl
                         else (a, b, c, d) : quadriate tl

-- Решение
rhyme :: [String] -> Int -> String
rhyme v k =
  case allEq of Just x -> x
                Nothing -> "NO"
  where quadl =  quadriate . map $ vowelk v
        allEq = foldr1 (\a b -> if b == a then b else Nothing) quadl

-- Main
main = do
  nk <- fmap words getLine
  let (n, k) = (nki !! 0, nki !! 1) where nki = map (\x -> read x :: Int) nk
  verse <- replicateM n getLine
  print . rhyme $ verse k
