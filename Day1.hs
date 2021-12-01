import Data.Foldable (foldl')

part1 :: [Int] -> Int
part1 (x:xs) =
  let
    (_, count) = foldl' accumulate (x, 0) xs
  in
    count
  where
  accumulate (prev, count) x
    | x > prev = (x, count + 1)
    | otherwise = (x, count)

window :: Int -> [a] -> [[a]]
window n xs
  | length w < n = []
  | otherwise = w : window n (tail xs)
  where
  (w, rest) = splitAt n xs

sumWindows :: Num a => Int -> [a] -> [a]
sumWindows n xs = map sum (window n xs)

part2 :: [Int] -> Int
part2 = part1 . sumWindows 3

main :: IO ()
main = do
  input <- readFile "day1"
  let nums = map read (lines input)
  print (part1 nums)
  print (part2 nums)
