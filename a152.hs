import Control.Applicative
import Control.Monad
import Data.List
import Data.Char

poss :: [Int] -> [Int]
poss [] = []
poss lst = elemIndices max lst
  where max = head . map $ maximum lst

calc :: [String] -> Int
calc strs =
  let ints = [ map digitToInt str | str <- strs]
      maxposs = map poss ints in
  map (length . nub . (++)) maxposs
  
main = do
  nm <- fmap words getLine
  let nmi = map (\x -> read x :: Int) nm
      n = nmi !! 0
      --m = nmi !! 1
  slist <- replicateM n getLine
  mapM print . show . calc $ slist