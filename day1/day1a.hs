main :: IO Int
main = do
  input <- readFile "input.txt"
  return $ fst $ zeroes 50 input 0

zeroes :: Int -> String -> Int -> (Int, Int)
zeroes n "" z = (add1if0 z n, n)
zeroes n (x : xs) z = case xs of
  a : b : c : '\n' : cs -> zeroes (turndial n x [a, b, c]) cs (add1if0 z n)
  a : b : c : "" -> zeroes (turndial n x [a, b, c]) "" (add1if0 z n)
  a : b : '\n' : cs -> zeroes (turndial n x [a, b]) cs (add1if0 z n)
  a : b : "" -> zeroes (turndial n x [a, b]) "" (add1if0 z n)
  a : '\n' : cs -> zeroes (turndial n x [a]) cs (add1if0 z n)
  a : "" -> zeroes (turndial n x [a]) "" (add1if0 z n)
  _ -> (-1, -1)

add1if0 :: Int -> Int -> Int
add1if0 z 0 = z + 1
add1if0 z _ = z

turndial :: Int -> Char -> String -> Int
turndial cur 'L' n = (cur - read n) `mod` 100
turndial cur _ n = (cur + read n) `mod` 100

-- >>> zeroes 50 "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82" 0
-- (3,32)
