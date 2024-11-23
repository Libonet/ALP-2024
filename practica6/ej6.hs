
import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)

newtype State s a = St { runState :: s -> (a,s) }

instance (Show s, Monoid s, Show a) => Show (State s a) where
  show (St f) = let (a,s) = f mempty in "\\s -> (" ++ show a ++ "," ++ show s ++ ")"

instance Monad (State s) where
  return x     = St (\s -> (x,s))
  (St h) >>= f = St (\s -> let (x,s') = h s
                           in runState (f x) s')
instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

set :: s -> State s ()
set s = St (\_ -> ((), s))

get :: State s s
get = St (\s -> (s,s))

