import Data.List

moveSanta :: (Int, Int) -> Char -> (Int, Int)
moveSanta (x, y) '^' = (x, y + 1)
moveSanta (x, y) 'v' = (x, y - 1)
moveSanta (x, y) '>' = (x + 1, y)
moveSanta (x, y) '<' = (x - 1, y)
moveSanta (x, y) _ = (x, y)

visitHouses :: String -> [(Int, Int)] -> [(Int, Int)]
visitHouses path history
  | path == "" = history
  | otherwise =
      visitHouses (tail path) newHistory
  where
    newHistory = history ++ [moveSanta (last history) (head path)]

visitHouses' :: String -> ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
visitHouses' path history
  | path == "" = history
  | otherwise = visitHouses' (tail path) newHistory
  where
    santaHistory = fst history
    robotHistory = snd history
    isSanta'sTurn = length santaHistory <= length robotHistory
    newHistory
      | isSanta'sTurn =
          ( santaHistory
              ++ [moveSanta (last santaHistory) (head path)],
            robotHistory
          )
      | otherwise =
          ( santaHistory,
            robotHistory
              ++ [moveSanta (last robotHistory) (head path)]
          )

main = do
  contents <- readFile "input.txt"

  let history = visitHouses contents [(0, 0)]
  let uniqueHouses = nub history
  let count = length uniqueHouses
  print count

  let history' = visitHouses' contents ([(0, 0)], [(0, 0)])
  -- let uniqueHouses = nub (fst history ++ snd history)
  let uniqueHouses' = nub (uncurry (++) history')
  let count' = length uniqueHouses'
  print count'