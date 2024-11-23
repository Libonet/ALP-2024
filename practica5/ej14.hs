{- 
Sea M una monada. Dados los operadores:
  
(>>)  :: M a -> M b -> M b
(>>=) :: M a -> (a -> M b) -> M b

a) de ser posible, escribir (>>) en funcion de (>>=)
b) de ser posible, escribir (>>=) en funcion de (>>)
-}

{- 
a)
a >> b = a >>= (\_ -> b)

b)
M (a) >>= f = M (a) >> f a
-}

