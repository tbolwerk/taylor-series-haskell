{-# LANGUAGE FlexibleContexts #-}

{-
Tail recursion used.
-}
factorial :: (Integral a, Num b) => a -> b
factorial n = fromIntegral (f n 1)
 where f 0 acc = acc
       f n acc = f (n-1) (acc*n)

e :: (Floating a) => a -> a 
e x = foldr (\i r -> x ^ i / factorial i + r) 0 [0..100]
sin' :: (Floating a) => a -> a
sin' x =  foldr (\i r-> ((-1) ^ i / factorial (2*i+1)) * x^(2*i + 1) + r) 0 [0..100]
cos' :: (Floating a) => a -> a
cos' x = foldr (\i r -> ((-1) ^ i / factorial (2*i)) * x^(2*i) + r) 0 [0..100]
