data BinTree a = Leaf | Bin a (BinTree a) (BinTree a)

foldBin :: BinTree a → b → (a → b → b → b) → b
foldBin Leaf l b = l
foldBin (Bin a t u) l b = b a (foldBin t l b) (foldBin u l b)

{- a) Definir en Haskell las siguientes funciones en t´erminos de foldBin:
i. isLeaf :: BinTree a → Bool, que determina si un ´arbol es una hoja;
ii. mapBin :: (a → b) → BinTree a → BinTree b que aplica la funci´on argumento a todos los elementos de
tipo a en el ´arbol;
iii. heightBin que devuelve la altura del ´arbol;
iv. mirrorBin que devuelve el ´arbol espejo.
-}

isLeaf :: BinTree a → Bool
isLeaf tree = foldBin tree True (\a l r -> False)

mapBin :: (a → b) → BinTree a → BinTree b
mapBin f t = foldBin t Leaf (\a l r -> (Bin (f a) l r))

heightBin :: BinTree a -> Int
heightBin t = foldBin t 0 (\a l r -> 1 + (max l r))

mirrorBin :: BinTree a -> BinTree a
mirrorBin t = foldBin t Leaf (\a l r -> (Bin a r l))