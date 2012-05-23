mindiv :: Int -> [Int]
mindiv 1 = [1]
mindiv x = x : (mindiv $ x `div` (head . reverse $ x : reverse [y | y <- [2 .. floor (sqrt $ fromIntegral x)], x `rem` y == 0 ]))

calc :: Int -> Int
calc n = sum $ mindiv n

main = do
	n <- fmap (\x -> read x :: Int) getLine
	print $ calc n