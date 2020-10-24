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
vapp (VLam f) v2 = f v2
vapp (VNeutral n) v2 = VNeutral (NApp n v2)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free n) (gEnv, _) = v where Just v = lookup n gEnv
-- E-App 1 y E-App2
eval' (t1 :@: t2) envs = let
                            v1 = eval' t1 envs
                            v2 = eval' t2 envs
                          in vapp v1 v2
-- Regla E-Abs
eval' (Lam t) (gEnv, lEnv) = VLam (\x -> eval' t (gEnv, x:lEnv))

-- Las implementaciones de las reglas E-App1, E-App2 y E-Abs conforman
-- la regla E-AppAbs por la evaluación de Haskell

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = quote' v 0

quote' :: Value -> Int -> Term
quote' (VNeutral (NFree (Global n))) _ = Free (Global n)
quote' (VNeutral (NFree (Quote k))) i = Bound (i - k - 1)
quote' (VNeutral (NApp n v)) i = (quote' (VNeutral n) i) :@: (quote' v i)
quote' (VLam f) i = let
                      v' = f (VNeutral (NFree (Quote i)))
                      q = quote' v' (i + 1)
                    in (Lam q)