import Control.Monad

maxparty :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -> Int
maxparty known fc ec friends emenies =
	undefined

-- helpers
getInt = \x -> read x :: Int
getPair str = (getInt $ lst !! 0, getInt $ lst !! 1) where lst = words str

-- IO
main = do
	n <- fmap getInt getLine -- known people total
	k <- fmap getInt getLine -- friend pairs count
	fpairs <- replicateM k getLine
	let friends = map getPair fpairs -- friend pairs
	m <- fmap getInt getLine -- enemy pairs count
	epairs <- replicateM m getLine
	let enemies = map getPair epairs -- enemy pairs
	print $ maxparty n k m friends enemies