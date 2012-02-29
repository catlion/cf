import Control.Applicative

readi' :: String -> Int
readi' s = read s :: Int

main = do
  nm <- fmap words getLine
  let nmi = map readi' nm
      studs <- mapM
  --mapM_ print nmi
  return ()