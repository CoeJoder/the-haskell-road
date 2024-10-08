module GS 

where 

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld n = ldf 2 n

ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

prime0 n | n < 1 = error "not a positive integer"
         | n == 1 = False
         | otherwise = ld n == n
