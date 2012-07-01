import Data.List

maxpal :: String -> String
maxpal src =
    maximum $ filter ispal subs
    where subs = subsequences src
          ispal x = x == (reverse x)

main = do
    src <- getLine
    putStrLn $ maxpal src