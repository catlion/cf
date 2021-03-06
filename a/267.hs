-- http://codeforces.ru/problemset/problem/267/A
import Data.List
import Control.Monad
-- helpers
getInt = \x -> read x :: Int
getPair str = (getInt $ lst !! 0, getInt $ lst !! 1) where lst = words str

solve :: (Int,Int) -> Int
solve (_,0) = 0
solve (0,_) = 0
solve (x,y) =
  acc' + (solve (mn,rm))
  where
    (mn, mx) = (min x y, max x y)
    (acc', rm) = mx `divMod` mn

main = do
  count <- fmap getInt getLine
  pairs <- replicateM count getLine
  let ps = map getPair pairs
      ss = map solve ps
  mapM_ putStrLn $ map show ss
{-
Заданы два числа. До тех пор, пока оба они больше нуля, с ними производят одну и ту же операцию: из большего числа вычитают меньшее. Если числа равны, то из одного вычитают другое. Например, из пары (4,17) за одну операцию получается пара (4,13), а из пары (5,5) пара (0,5).

Вам задано некоторое количество пар (ai, bi). Сколько операций будет выполнено для каждой из них?

Входные данные
В первой строке задано количество пар n (1  ≤  n  ≤  1000). Далее идут n строк, каждая содержит пару целых положительных чисел ai, bi (1  ≤  ai,  bi  ≤  109).

Выходные данные
Выведите искомое количество операций для каждой пары на отдельной строке.

Примеры тестов
входные данные
2
4 17
7 987654321
выходные данные
8
141093479
-}