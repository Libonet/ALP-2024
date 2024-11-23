{- 
a) Probar que String es un monoide 

instance Monoid (String) where
  mempty = ""
  mappend = (++)

Monoid.1 (mappend asociativa): (a `mappend` b) `mappend` c = a `mappend` (b `mappend` c)
Probamos por induccion de String en a:

a = "":
("" `mappend` b) `mappend` c
= <mappend>
("" ++ b) ++ c
= <++. 1>
b ++ c
= <++. 1>
"" ++ (b ++ c)
= <mappend>
"" `mappend` (b `mappend` c)
Listo...

HI: vale [(xs `mappend` b) `mappend` c = xs `mappend` (b `mappend` c)]

a = (x:xs)
((x:xs) `mappend` b) `mappend` c
= <mappend>
((x:xs) ++ b) ++ c
= <++. 2>
((x : (xs ++ b)) ++ c
= <++. 2>
x : ((xs ++ b) ++ c)
= HI
x : (xs ++ (b ++ c))
= <++.2>
x:xs ++ (b ++ c)
Listo.

Queda probado Monoid.1 para todo String.

Monoid.2 (mempty elemento neutro por ambos lados): mempty ++ a = a ++ mempty = a
Probamos por induccion de String en a:

a = "":
mempty ++ ""     "" ++ mempty
= <mempty>
"" ++ ""       "" ++ ""
= <++. 1>
""             ""
Listo...

HI: vale [mempty ++ xs = xs ++ mempty = xs]

a = (x:xs):
mempty ++ (x:xs)     
= <mempty>
"" ++ (x:xs)         
= <++. 1>            
x:xs                 
listo el primero.

(x:xs) ++ mempty
= <mempty>
(x:xs) ++ ""
= <++. 2>
x : (xs ++ "")
= HI
x:xs
Listo el segundo.

Queda probado Monoid.2 para todo String.


Por lo tanto String es un monoide.

-}

{- 
b)
-}
import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)

newtype Output w a = Out { runOut :: (a, w)}

instance Monoid w => Functor (Output w) where
  fmap = liftM

instance Monoid w => Applicative (Output w) where
  pure = return
  (<*>) = ap

instance Monoid w => Monad (Output w) where
  return x = Out (x, mempty)
  Out (x,w) >>= f = let Out (x', w') = f x
                    in Out (x', w `mappend` w')

{- 
Debemos probar que Output w a cumple las 3 leyes monadicas.

Monad.1: return a >>= k = k a

return a >>= k
= <return>
Out (a, mempty) >>= k
= <'>>='>
let Out (x',w') = k a
in Out (x', mempty `mappend` w')
= <mappend>
let Out (x',w') = k a
in Out (x',w')
= <Lema let: Out (x',w') = k a>
k a
Listo.

Monad.2: m >>= return = m

m >>= return 
= <return>
m >>= \x -> Out (x, mempty)
= <'>>=', supongo m = Out (a,w)>
let Out (x',w') = (\x -> Out (x, mempty)) a
in Out (x', w `mappend` w')
= <App>
let Out (x',w') = Out (a, mempty)
in Out (x', w `mappend` w')
= <Lema let: x'=a, w'=mempty>
Out (a, w `mappend` mempty)
= <mappend>
Out (a,w) = m
Listo.

Monad.3: m >>= (\x -> k x >>= h) = (m >>= k) >>= h

m >>= (\x -> k x >>= h)
= <'>>=', supongo m = Out (a,w)>
let Out (x',w') = (\x -> k x >>= h) a
in Out (x', w `mappend` w')
= <App>
let Out (x',w') = k a >>= h
in Out (x', w `mappend` w')
= <'>>=', supongo k a = Out (g a, w2)>
let Out (x',w') = 
              let Out (x2, w3) = h (g a)
              in Out (x2, w2 `mappend` w3)
in Out (x', w `mappend` w')
= <Lema let: x' = x2, w' = w2 `mappend` w3>
Out (x2, w `mappend` (w2 `mappend` w3))

(m >>= k) >>= h
= <'>>=', supongo m = Out (a,w)>
let Out (x',w2) = k a
in Out (x', w `mappend` w2) >>= h
= <'>>='>
let Out (x',w2) = k a
in let Out (x2, w3) = h x'
   in Out (x2, (w `mappend` w2) `mappend` w3)
= <mappend asociativa>
let Out (g a,w2) = k a
in let Out (x2, w3) = h (g a)
   in Out (x2, w `mappend` (w2 `mappend` w3))

Llegamos al mismo resultado???


Queda probado que Output w a es una monada.

-}

{-
c) Dar una instancia diferente de Monad para el mismo tipo. Esto prueba que un mismo tipo de datos puede tener diferentes instancias de monadas.

instance Monoid w => Monad (Output w) where
  return x = Out (x, mempty)
  Out (a,w) >>= f = Out (let Out (x',w') = f a
                         in Out (x', w' `mappend` w))
-}

{- d) Definir una operacion write :: Monoid w => w -> Output w () -}
write :: Monoid w => w -> Output w ()
write w = Out ((), w)

{- e) usando Output String, modificar el evaluador monadico basico de la teoria para
 - agregar una traza de cada operacion. Por ejemplo:
 - > eval (Div (Con 14) (Con 2))
 - El termino (Con 14) tiene valor 14
 - El termino (Con 2) tiene valor 2
 - El termino (Div (Con 14) (Con 2)) tiene valor 7
 - 7
 - -}

instance (Show a, Show w) => Show (Output w a) where
  show (Out (a,w)) = show w ++ show a

printEval :: Exp -> IO ()
printEval exp = putStrLn . snd . runOut $ eval exp

data Exp = Con Int     -- enteros
        |  Add Exp Exp -- sumas
        |  Div Exp Exp -- divisiones
        deriving Show

traza :: Exp -> Int -> String
traza term val = "El termino (" ++ show term ++ ") tiene valor " ++ show val ++ "\n"

writeTrace e =
  do x <- eval' e
     write $ traza e x
     return x

class Monad m => MonadThrow m where
  throw :: m ()

instance MonadThrow (Output String) where
  throw = do write "Error al evaluar\n"
             

eval :: Exp -> Output String Int
eval e = do v <- eval' e
            write $ show v
            return v

eval' :: Exp -> Output String Int
eval' (Con n) = Out (n,"")

eval' t@(Add a b) =
  do x <- writeTrace a
     y <- writeTrace b
     v <- return (x + y)
     write $ traza t v
     return v

eval' t@(Div a b) =
  do x <- writeTrace a
     y <- writeTrace b
     if y==0
     then do throw
             return (-1)
     else do v <- return (div x y)
             write $ traza t v
             return v


