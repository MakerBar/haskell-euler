{-|
 - A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 -
 - a2 + b2 = c2
 - For example, 32 + 42 = 9 + 16 = 25 = 52.
 -
 - There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 - Find the product abc.
 - -}

tripletProduct n = head $ take 1 [a*b*c | a <- [1..n-2], b <- [a..(n-a-1)], c <- [n-a-b], a^2 + b^2 == c^2]
--tripletProduct 1000
