

Leaf = \x f . x

Bin = \x l r c f . (f x (l c f) (r c f))

FoldBin = \x . x

i. isLeaf :: BinTree a → Bool, que determina si un ´arbol es una hoja;
ii. mapBin :: (a → b) → BinTree a → BinTree b que aplica la funci´on argumento a todos los elementos de
tipo a en el ´arbol;
iii. heightBin que devuelve la altura del ´arbol;
iv. mirrorBin que devuelve el ´arbol espej

isLeaf t = FoldBin tree True (\a l r -> False) = \t . (\x . x) t True (\a l r . False)

isLeaf Leaf = (\t . (\x . x) t True (\a l r . False)) Leaf -> (\x . x) Leaf True (\a l r . False) -> Leaf True (\a l r . False) -> True
isLeaf (Bin a l r) = (\t . (\x . x) t True (\a l r . False)) (Bin a l r) -> (\x . x) (Bin a l r) True (\a l r . False) -> (Bin a l r) True (\a l r . False) -> (\a l r . False) a (l True (\a l r . False)) (r True (\a l r . False)) -> False

mapBin f t = foldBin t Leaf (\a l r -> (Bin (f a) l r)) = (\x . x) t (\x f . x) (\a l r -> (Bin (f a) l r))
mapBin = \f t . (\x . x) t Leaf (\a l r . (Bin (f a) l r))

mapBin f Leaf = (\f t . (\x . x) t Leaf (\a l r . (Bin (f a) l r))) f Leaf -> (\x . x) Leaf Leaf (\a l r . (Bin (f a) l r)) -> Leaf Leaf (\a l r . (Bin (f a) l r)) -> Leaf
mapBin f (Bin a l r) -> (\f t . (\x . x) t Leaf (\a l r . (Bin (f a) l r))) f (Bin a l r) -> (\x . x) (Bin a l r) Leaf (\a l r . (Bin (f a) l r)) -> (Bin a l r) Leaf (\a l r . (Bin (f a) l r)) -> ((\a l r . (Bin (f a) l r))) x (l c ((\a l r . (Bin (f a) l r)))) (r c ((\a l r . (Bin (f a) l r)))) -> Bin (f a) (l c ((\a l r . (Bin (f a) l r)))) (r c ((\a l r . (Bin (f a) l r))))