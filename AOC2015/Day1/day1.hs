import Control.Monad
import System.IO

-- For problem 1
calcFloor ("", count) = ("", count)
calcFloor (h : t, count) = calcFloor (t, newCount)
  where
    newCount = count + goUpOrDown h

-- For Problem 2
calcFloor' ("", dist, count) = dist
calcFloor' (h : t, dist, count)
  | newCount >= 0 = calcFloor' (t, newDist, newCount)
  | otherwise = newDist
  where
    newCount = count + goUpOrDown h
    newDist = dist + 1

goUpOrDown :: Num a => Char -> a
goUpOrDown character
  | character == '(' = 1
  | otherwise = -1

main = do
  contents <- readFile "input.txt"
  let return = calcFloor' (contents, 0, 0)
  -- let return = calcFloor' (contents, 0, 0)
  print return