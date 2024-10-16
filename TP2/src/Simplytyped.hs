module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

data BST a = BE | BN a Int (BST a) (BST a)

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion t = conversion' t BE

conversion' :: LamTerm -> BST String -> Term
conversion' (LVar str) tree = let d = distance str tree
                              in if d /= -1 then Bound d else Free (Global str)
conversion' (LApp t1 t2) tree = (conversion' t1 tree) :@: (conversion' t2 tree)
conversion' (LAbs str typeName term) tree = let tree1 = increaseDistance tree
                                                tree2 = addTotree str tree1
                                            in Lam typeName (conversion' term tree2)

distance :: String -> BST String -> Int
distance str BE = -1
distance str (BN a d l r) | str < a = distance str l
                          | str > a = distance str r
                          | otherwise = d

addTotree :: String -> BST String -> BST String
addTotree str BE = (BN str 0 BE BE)
addTotree str (BN a d l r) | str < a = BN a d (addTotree str l) r
                           | str > a = BN a d l (addTotree str r)
                           | otherwise = BN a 0 l r

increaseDistance :: BST String -> BST String
increaseDistance BE = BE
increaseDistance (BN a d l r) = BN a (d+1) (increaseDistance l) (increaseDistance r)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value
eval = undefined




----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu


