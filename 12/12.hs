triangle :: Integral a => a -> a
triangle n
  | n == 1 = 1
  | otherwise = n + triangle (n-1)

factors :: Integral a => a -> [a]
factors n = factors2 n 1

factors2 :: Integral a => a -> a -> [a]
factors2 num div
  | div > num = []
  | mod num div == 0 = div : factors2 num (div + 1)
  | otherwise = factors2 num (div + 1)

-- last (head (take 1 [a | a <- (map factors (map triangle [1..])), length a > 500]))
