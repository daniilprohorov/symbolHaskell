{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}


module Main where

data Expr x = Add (Expr x) (Expr x) | Sub (Expr x) (Expr x) | Mul (Expr x) (Expr x) | Pow (Expr x) (Expr x) | Val x | S String deriving (Eq, Show)

-- Val c [a^2*b^2...]
--data Summ x =[(Expr x, [Expr x])] deriving (Show) 

--instance (Num x, Show x) => Show (Expr x) where
--	show (Add a b) = "( " ++ show a ++ " + " ++ show b ++ " )"
--	show (Sub a b) = "( " ++ show a ++ " - " ++ show b ++ " )"
--	show (Mul a b) = "( " ++ show a ++ " * " ++ show b ++ " )"
--	show (Pow n x) = "( " ++ show x ++ " )^" ++ show n
--	show (S s) = show s	
--	show (Val x) = show x

--transform :: Expr Int -> Expr Int
transform (Sub a b) = trOrReturn $ Add a (Mul b (Val (-1)))
transform (Add a (Val 0)) = trOrReturn a
transform (Add (Val 0) a) = trOrReturn a
transform (Mul a (Val 1)) = trOrReturn a
transform (Mul (Val 1) a) = trOrReturn a
transform (Pow (Val 1) a) = trOrReturn a
transform (Pow (S s) a) = Pow (S s) (trOrReturn a)
transform (Pow n a) = trOrReturn $ Mul a (Pow (evaluate $ Sub n (Val 1)) a)
transform (Mul (Add a b) (Add c d)) = 
	trOrReturn $ 
		Add 
		(trOrReturn $ 
			Add 
			(trOrReturn $ Mul (trOrReturn a) (trOrReturn c)) 
			(trOrReturn $ Mul (trOrReturn a) (trOrReturn d))) 
		(trOrReturn $ 
			Add 
			(trOrReturn $ Mul (trOrReturn b) (trOrReturn c)) 
			(trOrReturn $ Mul (trOrReturn b) (trOrReturn d)))
transform (Mul a (Add b c)) = trOrReturn $ 
	Add 
	(trOrReturn $ Mul (trOrReturn a) (trOrReturn b))
	(trOrReturn $ Mul (trOrReturn a) (trOrReturn c))
transform (Mul (Add a b) c) = trOrReturn $ 
	Add 
	(trOrReturn $ Mul (trOrReturn a) (trOrReturn c))
	(trOrReturn $ Mul (trOrReturn b) (trOrReturn c))

transform (Add a b) = Add (trOrReturn a) (trOrReturn b)
transform (Sub a b) = Sub (trOrReturn a) (trOrReturn b)
transform (Mul a b) = Mul (trOrReturn a) (trOrReturn b)
transform (Val n) = Val n
transform x = x 

tryTransform 0 expr = expr
tryTransform n expr = tryTransform (n-1) (transform expr)

--trOrReturn :: Expr Int -> Expr Int 
trOrReturn expr 
    | expr == (transform expr) = expr
    | otherwise                = transform expr

--evaluate :: Expr Int -> Expr Int
evaluate (Add a b) = evAdd (evaluate a) (evaluate b)
evaluate (Sub a b) = evSub (evaluate a) (evaluate b)
evaluate (Mul a b) = evMul (evaluate a) (evaluate b)
evaluate x = x 

--evAdd :: Expr Int -> Expr Int -> Expr Int
evAdd (Val a) (Val b) = Val (a + b)
evAdd a b = Add a b

--evSub :: Expr Int -> Expr Int -> Expr Int
evSub (Val a) (Val b) = Val (a - b)
evSub a b = Sub a b

--evMul :: Expr Int -> Expr Int -> Expr Int
evMul (Val a) (Val b) = Val (a * b)
evMul a b = Mul a b

mulToList (Mul (S a) (S b)) = [S a, S b] 
mulToList (Mul (S a) (Val b)) = [S a, Val b] 
mulToList (Mul (Val a) (S b)) = [Val a, S b] 
mulToList (Mul (Val a) (Val b)) = [evaluate (Mul (Val a)(Val b))] 
mulToList (Mul a b) = mulToList a ++ mulToList b 
mulToList x = [x] 

toFlat (Mul a b) = [mulToList (Mul a b)]
toFlat (Add a b) = toFlat a ++ toFlat b  
toFlat x = [[x]]  

isVal (Val x) = True
isVal n       = False

getVal (Val x) = x

isS (S s) = True
isS n     = False

getS (S s) = s

toSumm lst = 
	((product $ map getVal $ filter (isVal) lst), map getS $ filter isS lst)

smm flat = map toSumm flat

isPatternC [] pattern = 0
isPatternC ((c, lst):xs) pattern =
	if (snd $ pattern) == lst 
	then c + isPatternC xs pattern 
	else isPatternC xs pattern 


patternConcat_ [] smm          = []    
patternConcat_ (x:xs) smm = (isPatternC smm x, snd x) :patternConcat_ xs smm

notZero (0, x) = False
notZero n      = True 

sameConcat sm = patternConcat_ sm sm

isNotEqual a b = a /= b

deleteDuplicates_ []     b = b 
deleteDuplicates_ (x:xs) b = deleteDuplicates_ xs (x : filter (isNotEqual x) b)    

clear exprs = filter notZero exprs

deleteDuplicates exprs = deleteDuplicates_ (clear exprs) (clear exprs)

main :: IO ()
main = do
    print "kek"
    -- print $ test b'
    -- print $ test hb
