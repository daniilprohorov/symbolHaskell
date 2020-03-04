{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}


module Main where

data Expr x = Add (Expr x) (Expr x) | Sub (Expr x) (Expr x) | Mul (Expr x) (Expr x) | Pow (Expr x) (Expr x) | Val x deriving (Eq, Show)


transform :: Expr Int -> Expr Int
transform (Sub a b) = Add a (Mul b (Val (-1)))
transform (Add a (Val 0)) = a
transform (Add (Val 0) a) = a
transform (Mul a (Val 1)) = a
transform (Mul (Val 1) a) = a
transform (Pow (Val 1) a) = a
transform (Pow n a) = Mul a ()
transform (Mul (Add a b) (Add c d)) = Add (Add (Mul a c) (Mul a d)) (Add (Mul b c) (Mul b d))
transform (Mul a (Add b c)) = Add (Mul a b) (Mul a c)
transform (Mul (Add a b) c) = Add (Mul a c) (Mul b c)


evaluate :: Expr Int -> Int
evaluate (Add a b) = (evaluate a) + (evaluate b)
evaluate (Mul a b) = (evaluate a) * (evaluate b)
evaluate (Val a) = a

main :: IO ()
main = do
    print "kek"
    -- print $ test b'
    -- print $ test hb
