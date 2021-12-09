import Data.List (foldl', transpose)

avg :: [Int] -> Int
avg xs =
  let
    (count, sum) =
      foldr
        (\x (count, sum) ->
          (count + 1, sum + x))
        (0, 0)
        xs
  in
    div (sum + div count 2) count

least :: [Int] -> Int
least xs
  | avg xs == 1 = 0
  | avg xs == 0 = 1

decimal :: [Int] -> Int
decimal =
  foldl'
    (\acc x -> x + acc * 2)
    0

gamma :: [[Int]] -> Int
gamma =
  decimal .
  map avg .
  transpose

epsilon :: [[Int]] -> Int
epsilon =
  decimal .
  map least .
  transpose

oxygen :: [[Int]] -> Int
oxygen xss = go 0 xss
  where
  go n xss = do
    let
      mostCommon = avg (transpose xss !! n)
      criterion xs = (xs !! n) == mostCommon
    case filter criterion xss of
      [e] -> decimal e
      es -> go (n + 1) es

co2 :: [[Int]] -> Int
co2 xss = go 0 xss
  where
  go n xss = do
    let
      leastCommon = least (transpose xss !! n)
      criterion xs = (xs !! n) == leastCommon
    case filter criterion xss of
      [e] -> decimal e
      es -> go (n + 1) es

part1 :: [[Int]] -> (Int, Int)
part1 x = (gamma x, epsilon x)

part2 :: [[Int]] -> (Int, Int)
part2 x = (oxygen x, co2 x)

readDigit :: Char -> Int
readDigit '0' = 0
readDigit '1' = 1

test :: [[Int]]
test = map (map readDigit)
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

parse :: String -> [[Int]]
parse = map (map readDigit) . lines

main :: IO ()
main = do
  numbers <- readFile "input3"
  putStrLn $ "Test: "
  putStr $ "Part 1: "
  print $ part1 test
  putStr $ "Part 2: "
  print $ part2 test
  putStrLn $ "Puzzle: "
  putStr $ "Part 1: "
  print $ uncurry (*) $ part1 $ parse numbers
  putStr $ "Part 2: "
  print $ uncurry (*) $ part2 $ parse numbers
