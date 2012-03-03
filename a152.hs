import Control.Applicative
import Control.Monad
import Data.List
import Data.Char

poss :: [Int] -> [Int]
poss [] = []
poss lst = elemIndices mx lst
  where mx = maximum lst

toints :: [String] -> [[Int]]
toints = map . map $ digitToInt

calc :: [String] -> Int
calc = length . nub . foldl1 (++) . map poss . transpose . toints

main = do
  nm <- fmap words getLine
  let nmi = map (\x -> read x :: Int) nm
      n = nmi !! 0
      --m = nmi !! 1
  slist <- replicateM n getLine
  print $ calc slist