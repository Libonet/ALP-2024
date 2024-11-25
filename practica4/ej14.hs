{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

type List x = forall y. (x -> y -> y) -> y -> y 

nil :: forall x. (Show x) => List x
nil c n = n

cons :: forall x. (Show x) => x -> List x -> List x
cons x xs = \c n -> c x (xs c n)

type CBool = forall x. x -> x -> x

true :: CBool
true a b = a

false :: CBool
false a b = b


insert :: forall x. (Show x) => (x -> x -> CBool) -> List x -> x -> List x
insert f xs v = xs @(List x) (\y ys -> (f v y) (cons @x v (cons @x y ys)) (cons @x y ys)) nil


