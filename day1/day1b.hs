main :: IO Int
main = do
  input <- readFile "input.txt"
  return $ fst $ zeroes 50 input 0

countZeroesInSingleRotation :: Int -> Int -> Char -> Int -> (Int, Int)
countZeroesInSingleRotation res 0 _ zeroes = (res, zeroes)
countZeroesInSingleRotation cur amount direction zeroes =
  case cur of
    0 -> case direction of
      'L' -> countZeroesInSingleRotation 99 (amount - 1) direction (zeroes + 1)
      _ -> countZeroesInSingleRotation (cur + 1) (amount - 1) direction (zeroes + 1)
    99 -> case direction of
      'L' -> countZeroesInSingleRotation (cur - 1) (amount - 1) direction zeroes
      _ -> countZeroesInSingleRotation 0 (amount - 1) direction zeroes
    _ -> case direction of
      'L' -> countZeroesInSingleRotation (cur - 1) (amount - 1) direction zeroes
      _ -> countZeroesInSingleRotation (cur + 1) (amount - 1) direction zeroes

zeroes :: Int -> String -> Int -> (Int, Int)
zeroes n "" z = (z, n)
zeroes n (x : xs) z = case xs of
  a : b : c : '\n' : cs -> zeroes (fst (countZeroesInSingleRotation n (read [a, b, c]) x z)) cs (snd (countZeroesInSingleRotation n (read [a, b, c]) x z))
  a : b : c : "" -> zeroes (fst (countZeroesInSingleRotation n (read [a, b, c]) x z)) "" (snd (countZeroesInSingleRotation n (read [a, b, c]) x z))
  a : b : '\n' : cs -> zeroes (fst (countZeroesInSingleRotation n (read [a, b]) x z)) cs (snd (countZeroesInSingleRotation n (read [a, b]) x z))
  a : b : "" -> zeroes (fst (countZeroesInSingleRotation n (read [a, b]) x z)) "" (snd (countZeroesInSingleRotation n (read [a, b]) x z))
  a : '\n' : cs -> zeroes (fst (countZeroesInSingleRotation n (read [a]) x z)) cs (snd (countZeroesInSingleRotation n (read [a]) x z))
  a : "" -> zeroes (fst (countZeroesInSingleRotation n (read [a]) x z)) "" (snd (countZeroesInSingleRotation n (read [a]) x z))
  _ -> (-1, -1)

-- >>> zeroes 50 "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82" 0
-- (6,32)

-- >>> countZeroesInSingleRotation 95 60 'R' 0
-- (55,1)
