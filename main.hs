fact :: Int -> Int
fact n
  | n == 0 = 1
  | otherwise = n * fact (n - 1)

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  ( if x > 100
      then x
      else doubleMe x
  )
    + 1

someEvens :: Int -> Int -> [Int]
someEvens count lowerLimit = [x * 2 | x <- [1 .. count], x * 2 >= lowerLimit]

boomBang xs = [if x < 10 then "Bang" else "Boom" | x <- xs, odd x]

-- Really bad Fizz buzz

fizzReturn :: Int -> Int -> String -> String
fizzReturn num modBy returnStr = if mod num modBy == 0 then returnStr else ""

fizzBuzz :: Int -> [String]
fizzBuzz limit =
  [ if returnStr == ""
      then show x
      else returnStr
    | x <- [1 .. limit],
      let returnStr = fizzReturn x 3 "Fizz" ++ fizzReturn x 5 "Buzz"
  ]

getMax :: Ord a => [a] -> a
getMax [] = error "Empty list"
getMax [x] = x
getMax (x : xs) = max x maxTail
  where
    maxTail = getMax xs

addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

initials fname lname = f : ". " ++ l : "."
  where
    (f : _) = fname
    (l : _) = lname

calcBMIs xs = [bmi h w | (h, w) <- xs]
  where
    bmi h w = w / h ^ 2



quickSort [] = []
quickSort (x: xs) = smallarSorted ++ [x] ++ largerSorted
  where 
    smallarSorted = quickSort [y| y <- xs, y <= x]
    largerSorted = quickSort [y| y <- xs, y > x]


calcFloor  ("", count) = count
calcFloor (h: t, count) = calcFloor(t, count + goUpOrDown h)

goUpOrDown :: Num a => Char -> a
goUpOrDown character 
  | character == '(' = 1
  | otherwise = -1
