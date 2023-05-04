import Control.Monad.Trans.RWS (state)

-- for p1
-- allOff = [[(x, y, False) | x <- [0 .. 999]] | y <- [0 .. 999]]

-- isIn :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
-- isIn (posX, posY) (startX, startY) (endX, endY)
--   | startX <= posX && posX <= endX && startY <= posY && posY <= endY = True
--   | otherwise = False

-- turnLightsOn state start end =
--   [ [ if isIn (x, y) start end then (x, y, True) else (x, y, val)
--       | (x, y, val) <- row
--     ]
--     | row <- state
--   ]

-- turnLightsOff state start end =
--   [ [ if isIn (x, y) start end then (x, y, False) else (x, y, val)
--       | (x, y, val) <- row
--     ]
--     | row <- state
--   ]
-- end For p1

-- For p2
allOff :: [[(Int, Int, Int)]]
allOff = [[(x, y, 0) | x <- [0 .. 999]] | y <- [0 .. 999]]

isIn :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isIn (posX, posY) (startX, startY) (endX, endY)
  | startX <= posX && posX <= endX && startY <= posY && posY <= endY = True
  | otherwise = False

turnLightsOn state start end =
  [ [ if isIn (x, y) start end then (x, y, val + 1) else (x, y, val)
      | (x, y, val) <- row
    ]
    | row <- state
  ]

turnLightsOff state start end =
  [ [ if isIn (x, y) start end then (x, y, max (val - 1) 0) else (x, y, val)
      | (x, y, val) <- row
    ]
    | row <- state
  ]

toggleLights state start end =
  [ [ if isIn (x, y) start end then (x, y, val + 2) else (x, y, val)
      | (x, y, val) <- row
    ]
    | row <- state
  ]

-- end For p2

-- Parse the string of type
data ActionType = Toggle | TurnOn | TurnOff deriving (Eq, Ord, Enum, Show)

isNum x = x <= '9' && x >= '0'

isNotNum x = not (isNum x)

parseCommand string = (action, (startX, startY), (endX, endY))
  where
    actionPart = takeWhile isNotNum string
    actionLen = length actionPart
    action
      | actionLen == 7 = Toggle
      | actionLen == 8 = TurnOn
      | otherwise = TurnOff

    startX = read (takeWhile (/= ',') (drop actionLen string)) :: Int
    startY = read (takeWhile (/= ' ') (tail (dropWhile (/= ',') (drop actionLen string)))) :: Int
    endStr = tail (dropWhile (/= ' ') (tail (dropWhile (/= ' ') (drop actionLen string))))
    endX = read (takeWhile (/= ',') endStr) :: Int
    endY = read (tail (dropWhile (/= ',') endStr)) :: Int

applyCommand :: (Num c, Ord c) => [(ActionType, (Int, Int), (Int, Int))] -> [[(Int, Int, c)]] -> [[(Int, Int, c)]]
applyCommand [] state = state
applyCommand commands state =
  applyCommand (tail commands) result
  where
    (action, start, end) = head commands
    result
      | action == Toggle = toggleLights state start end
      | action == TurnOff = turnLightsOff state start end
      | action == TurnOn = turnLightsOn state start end
      | otherwise = state

-- sumTurnedOnLight :: [[(Int, Int, Bool)]] -> Int
-- sumTurnedOnLight state = sum [sum row | row <- stateInt]
--   where
--     stateInt = [[if val then 1 else 0 | (x, y, val) <- row] | row <- state]

sumTurnedOnLight :: [[(Int, Int, Int)]] -> Int
sumTurnedOnLight state = sum [sum row | row <- stateInt]
  where
    stateInt = [[val | (x, y, val) <- row] | row <- state]

main = do
  contents <- readFile "input.txt"
  let commands = map parseCommand (lines contents)
  let finalState = applyCommand commands allOff
  let sum = sumTurnedOnLight finalState
  print sum