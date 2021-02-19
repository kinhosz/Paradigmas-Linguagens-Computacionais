answer :: Int
answer = 45

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int
square x = x*x

vendas :: Int -> Int
vendas n = mod n 17

totalVendas :: Int -> Int
totalVendas n | n == 0      = 0
              | n > 0       = vendas n + totalVendas (n-1)
              | otherwise   = 0

imc :: Double -> Double -> Double
imc n m = n / (m*m)

allLess :: Int -> Int -> Int -> Bool
allLess a b c = (a < b && b < c)

vendasIguais :: Int -> Int -> Int
vendasIguais s n | n < 0         = 0
                 | vendas n == s = 1 + vendasIguais s (n-1)
                 | otherwise     = vendasIguais s (n-1)

check :: Int -> Int -> Bool
check p i | i*i > p      = True
          | mod p i == 0 = False
          | otherwise    = check p (i+1)

isPrime :: Int -> Bool
isPrime p = check p 2

primosEntreSi :: Int -> Int -> Bool
primosEntreSi x y | y == 0 && x == 1   = True
                  | y == 0             = False
                  | otherwise          = primosEntreSi y (mod x y)

fat :: Int -> Int
fat n | n == 0    = 1
      | otherwise = n * fat (n-1)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b && b == c && c == d)

equalCount :: Int -> Int -> Int -> Int -> Int
equalCount a b c d | all4Equal a b c d     = 4
                   | all4Equal a b c c     = 3
                   | all4Equal a b b d     = 3
                   | all4Equal b b c d     = 3
                   | all4Equal a a b b     = 2
                   | all4Equal a a c c     = 2
                   | all4Equal a a d d     = 2
                   | all4Equal b b c c     = 2
                   | all4Equal b b d d     = 2
                   | all4Equal c c d d     = 2
                   | otherwise             = 1