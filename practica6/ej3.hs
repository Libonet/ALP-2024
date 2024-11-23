module Main where
import System.IO

main :: IO ()
main = do secret <- getFromPrompt "Ingrese un numero secreto: "
          putStrLn "Adivine el numero ingresado :D"
          guess secret

getFromPrompt :: String -> IO Int
getFromPrompt s =
  do putStrLn s
     hSetEcho stdin False
     n <- getLine
     hSetEcho stdin True
     return (read n)


guess :: Int -> IO ()
guess secret =
--  do num <- getInputNum
  do num <- read <$> getLine
     case compare num secret of
      EQ -> putStrLn "Lo adivinaste!!! :D"
      LT -> do putStrLn "El numero secreto es mas grande :c"
               guess secret
      GT -> do putStrLn "El numero secreto es mas chico :c"
               guess secret

{-  
getInputNum :: IO Int
getInputNum = 
  do input <- getLine
     return $ read input
-}

{-  
Esto está definido en el Preludio

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) f m = 
  do v <- m
     return $ f v
-}

