import Data.List

data Ch = Maybe Char
data Rhyme = Maybe String
-- Current state in 4-liner
data Pos = Start 
         | First Ch
         | Sec (Ch, Ch)
         | Thr (Ch, Ch, Ch)
         | Fin (Ch, Ch, Ch, Ch)
-- Accumulator in fold
data State = State { k :: Int,
                     pos :: Pos,
                     rhyme :: [Rhyme] }

-- k-th vowel from the end of string
vowelk :: String -> Int -> Ch
vowelk str k =
  if len > pos then Just (f !! (reverse pos))
  else Nothing
    where f = filter (\c -> elem c v) str
          v = ['a','e','i','o','u']
          pos = k + 1
          len = length f

getr :: (Ch, Ch, Ch, Ch) -> Rhyme
-- Rhyme in part
quadrhyme :: (Char, Char, Char, Char) -> Maybe String
quadrhyme (a, b, c, d) =
  case length x of 1 -> Just "aaaa"
                   2 -> case (a == b, a == c, a == d) of (True, _, _) -> Just "aabb"
                                                         (_, True, _) -> Just "abab"
                                                         (_, _, True) -> Just "abba"
                   _ -> Nothing
    where x = group $ sort [a, b, c, d]


folder' :: State -> String -> State
folder state line =
  case state.pos of Start -> state {pos = nv }
                    First x -> state { pos = (x, nv) }
                    Sec (a1, a2) -> state { pos = (a1, a2, nv) }
                    Thr (a1, a2, a3) -> state { pos = (a1, a2, a3, nv) }
                    Fin (a1, a2, a3, a4) -> state { pos = Start, rhyme = [curr:rhyme] } 
                                            where curr = quadrhyme (a1, a2, a3, a4)
    where nv = vowelk $ line state.k

-- Solve
rhyme :: [String] -> Int -> String
rhyme v k = foldl

-- Main
main = do
  nk <- fmap words getLine
  let (n, k) = (nki !! 0, nki !! 1) where nki = map (\x -> read x :: Int) nk
  verse <- replicateM n getLine
  print . rhyme $ verse k