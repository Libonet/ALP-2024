import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)

import Prelude hiding (Either, Just, Left)

newtype Id a = Id a deriving Show
data Either e a = Left e | Just a deriving Show

instance Monad Id where
    return = Id
    (Id x) >>= f = f x

instance Monad (Either e) where
    return = Just
    (Just x) >>= f = f x
    (Left x) >>= f = Left x

instance Functor Id where
    fmap = liftM

instance Applicative Id where
    pure = return
    (<*>) = ap


instance Functor (Either e) where
    fmap = liftM

instance Applicative (Either e) where
    pure = return
    (<*>) = ap
