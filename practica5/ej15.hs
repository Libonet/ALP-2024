import Prelude hiding (mapM)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (x:xs) = 
  do y <- f x
     ys <- mapM f xs
     return (y:ys)

test1 :: String -> IO String
test1 str =
  do n <- getLine
     putStrLn (str ++ n)
     return n



foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f e []     = return e
foldM f e [x]    = f e x
foldM f e (x:xs) = 
  do y <- f e x
     foldM f y xs

suma :: Int -> Int -> IO Int
suma a b = 
  do return (a + b)
