{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)

-- 3)

type Env = [(String, Bool)]
newtype Reader a = R { runR :: Env -> a } 

-- a) Dar la instancia de monad para Reader

instance Functor Reader where
  fmap = liftM

instance Applicative Reader where
  pure = return
  (<*>) = ap

instance Monad Reader where
  return x = R (\env -> x)
  R h >>= f = R (\env -> let x = h env
                         in runR (f x) env) 

-- b)

{-
ask :: Reader Env
ask = R id

local :: (Env -> Env) -> Reader a -> Reader a
local envF (R h) = R (\env -> let newEnv = envF env
                              in h newEnv)
-}

-- c)

type Traza = String
newtype ReaderWriter a = RW { runRW :: Env -> (a, Traza, Int) }

instance Functor ReaderWriter where
  fmap = liftM

instance Applicative ReaderWriter where
  pure = return
  (<*>) = ap

instance Monad ReaderWriter where
  return x = RW (\env -> (x, [], 0))
  RW h >>= f = RW (\env -> let (x, traza, pasos) = h env
                               RW g = f x
                               (x', traza', pasos') = g env
                           in (x', traza ++ traza', pasos + pasos'))

addTrace :: Traza -> ReaderWriter ()
addTrace s = RW (\_ -> ((),s,0))

addStep :: ReaderWriter ()
addStep = RW (\_ -> ((), "", 1))

ask :: ReaderWriter Env
ask = RW (\env -> (env,"",0))

local :: (Env -> Env) -> ReaderWriter a -> ReaderWriter a
local envF (RW h) = RW (\env -> let newEnv = envF env
                                in h newEnv)


-- d)

data BoolExp = T | F                        -- constantes
               | Var String                 -- variable booleana
               | Let String BoolExp BoolExp -- definir una variable local nueva
               | And BoolExp BoolExp
               | Not BoolExp
  deriving (Eq, Show)

find :: String -> Env -> Bool
find name []     = False
find name ((var,bool):xs) = if name == var
                            then bool
                            else find name xs

traceEval term res = show term ++ " --> " ++ show res ++ "\n"

eval :: BoolExp -> ReaderWriter Bool
eval T = RW (\env -> (True, "", 0))
eval F = RW (\env -> (False, "", 0))

eval (Var name) = 
  do current_env <- ask
     RW (\env -> (find name current_env, "", 0))

eval t@(Let name e1 e2) = 
  do res1 <- eval e1
     local (\env -> (name,res1):env) ask
     res2 <- eval e2
     addStep
     addTrace $ traceEval t res2
     return res2

eval t@(And e1 e2) =
  do res1 <- eval e1
     res2 <- eval e2
     res <- return (res1 && res2)
     addStep
     addTrace $ traceEval t res
     return res

eval t@(Not e1) =
  do res1 <- eval e1
     addStep
     addTrace $ traceEval t res1
     return res1

-- e) 

mostrar :: BoolExp -> IO ()
mostrar e = 
  do (res, traza, pasos) <- return $ runRW (eval e) []
     putStrLn $ "El resultado de la evaluacion es " ++ show res ++ "\n"
     putStrLn "Pasos de evaluacion:"
     putStrLn traza
     putStrLn $ "La expresion se evaluo en " ++ show pasos ++ " pasos."

