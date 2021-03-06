{-
Умный Бобер из ABBYY увлекся изучением квадратных матриц. Сейчас он занят исследованием матрицы размера n × n, где n — нечетно. Умный Бобер считает хорошими следующие элементы матрицы:

    Элементы главной диагонали.
    Элементы побочной диагонали.
    Элементы «средней» строки — строки, ниже которой и выше которой находится ровно по строк.
    Элементы «среднего» столбца — столбца, левее которого и правее которого находится ровно по столбцов. 

На рисунке изображена матрица 5 × 5. Зеленым цветом отмечены хорошие элементы.

Помогите Умному Бобру подсчитать сумму хороших элементов заданной матрицы.
Входные данные

Первая строка входных данных содержит единственное целое нечетное число n. Следующие n строк входных данных содержат по n целых чисел aij (0 ≤ aij ≤ 100), разделенных единичными пробелами, — элементы заданной матрицы.

Ограничения на входные данные для получения 30 баллов:

    1 ≤ n ≤ 5 

Ограничения на входные данные для получения 100 баллов:

    1 ≤ n ≤ 101 

Выходные данные

Выведите единственное целое число — сумму хороших элементов матрицы.
Примеры тестов
Входные данные

3
1 2 3
4 5 6
7 8 9

Выходные данные

45

Входные данные

5
1 1 1 1 1
1 1 1 1 1
1 1 1 1 1
1 1 1 1 1
1 1 1 1 1

Выходные данные

17 -}
import Data.List
--import Control.Applicative
import Control.Monad

poss :: Int -> [(Int, Int)]
poss n = [ (x,y) | x <- [0 .. n-1], y <- [0 .. n-1], x==y || (x+y)==(n-1) || x+1==mid' || y+1==mid']
  where mid' = (n `div` 2) + 1

positions :: Int -> [Int]
positions n = map flattered (poss n)
  where flattered (x,y) = (n * x) + y

sm :: [[Int]] -> Int -> Int
sm matrix n =
  foldl add' 0 poslist
  where poslist = positions n
        flatmatrix = foldl (++) [] matrix
        add' acc x = (flatmatrix !! x) + acc

main = do
  ns <- getLine
  let n = read ns :: Int
  slist <- replicateM n getLine
  let rint x = read x :: Int
  let imap = map rint . words
  let matrix = map imap slist
  print $ sm matrix n
