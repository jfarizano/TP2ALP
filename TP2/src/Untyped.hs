module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion t = conversion' t []

conversion' :: LamTerm -> [String] -> Term
conversion' (LVar s) xs = case elemIndex s xs of
                            Just n -> Bound n
                            Nothing -> Free (Global s)
conversion' (App lt1 lt2) xs = let
                                  t1 = conversion' lt1 xs
                                  t2 = conversion' lt2 xs
                                in (t1 :@: t2)
conversion' (Abs s lt) xs = Lam (conversion' lt (s:xs))                             

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp = undefined

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






