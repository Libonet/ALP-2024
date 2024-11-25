{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)
import Prelude hiding (mapM, fail)

newtype WriterMaybe w a = WM { runWM :: (Maybe a, [w])}

-- b)

instance Functor (WriterMaybe w) where
  fmap = liftM

instance Applicative (WriterMaybe w) where
  pure  = return
  (<*>) = ap

instance Monad (WriterMaybe w) where
  return x = WM (Just x, [])
  WM (Nothing, ws) >>= f = WM (Nothing, ws)
  WM (Just x, ws) >>= f  = let WM (r, ws2) = f x
                           in WM (r, ws ++ ws2)

-- c)
tell ws = WM (Just (), ws)

fail :: WriterMaybe e a
fail = WM (Nothing, [])

-- d)

type Rule = String
type Packet = String
data Result = Accepted | Rejected
  deriving (Show, Eq)

instance (Show w, Show a) => Show (WriterMaybe w a) where
  show (WM (x, ws)) = show x ++ ":\n" ++ show ws

match :: [Rule] -> Packet -> [(Rule, Result)]
match [] p = []
match (rule:rules) p
  | take 3 rule == "Not" && drop 3 rule == p = (rule, Rejected) : match rules p
  | rule==p = (rule, Accepted) : match rules p
  | otherwise = match rules p

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) =
  do y <- f x
     ys <- mapM f xs
     return (y:ys)

filterPacket :: [Rule] -> Packet -> WriterMaybe Char Packet
filterPacket rules p =
  let matches = match rules p
  in if matches == []
  then do tell $ "UNMATCHED PACKET " ++ p ++ "\n"
          fail
  else do bools <- mapM (compile p) matches
          if and bools then return p else fail

compile :: Packet -> (Rule, Result) -> WriterMaybe Char Bool
compile p (rule, res) =
  do tell $ "MATCHED " ++ p ++ " WITH RULE " ++ rule ++ "\n"
     if res == Accepted
     then do tell "RULE ACCEPTED\n"
             return True
     else do tell "RULE REJECTED\n"
             return False

