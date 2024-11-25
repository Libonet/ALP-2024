
List X = $\forall$ R. (X -> R -> R) -> R -> R

singleton: $\forall$ X. X -> List X
singleton = $\Gamma$ X. \x:X. cons x nil

{-

inits: [a] -> [[a]]
inits []     = []
inits (x:xs) = [x] : map (append [x]) inits(xs)

-}

inits : $\forall$ X. List X -> List (List X)
inits = $\Gamma$ X. \xs:List X. xs <List X> (\y:X. \ys:List X. cons <X> y (map <X> (append <X> (head <X> xs)) ys)) nil 
