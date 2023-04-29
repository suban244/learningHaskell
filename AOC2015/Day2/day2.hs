calcWrappingPaper :: (Int, Int, Int) -> Int
calcWrappingPaper (l, w, h) = 2 * s1 + 2 * s2 + 2 * s3 + slack
  where
    s1 = l * w
    s2 = w * h
    s3 = l * h
    slack = min (min s1 s2) s3

calcRibbonLength :: (Int, Int, Int) -> Int
calcRibbonLength (l, w, h) = minPerimeter + volume
  where
    minPerimeter = min (min (2 * l + 2 * w) (2 * w + 2 * h)) (2 * l + 2 * h)
    volume = l * w * h

calcTotal :: ((Int, Int, Int) -> Int) -> [(Int, Int, Int)] -> Int
calcTotal _ [] = 0
calcTotal f boxes = headCost + tailCost
  where
    h : t = boxes
    headCost = f h
    tailCost = calcTotal f t

-- Parse "Int x Int x Int" to (Int, Int, Int)
parseDimensions :: String -> (Int, Int, Int)
parseDimensions dimensions = (l, w, h)
  where
    l = read (takeWhile (/= 'x') dimensions) :: Int
    w = read (takeWhile (/= 'x') (tail (dropWhile (/= 'x') dimensions))) :: Int
    h = read (tail (dropWhile (/= 'x') (tail (dropWhile (/= 'x') dimensions)))) :: Int

main = do
  contents <- readFile "input.txt"
  let wrapperCost = calcTotal calcWrappingPaper (map parseDimensions (lines contents))
  let ribbonCost = calcTotal calcRibbonLength (map parseDimensions (lines contents))
  print wrapperCost
  print ribbonCost