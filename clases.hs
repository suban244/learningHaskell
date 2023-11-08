class Size a where
    empty :: a
    size :: a -> Int
    sameSize :: a -> a -> Bool

instance Size (Maybe a) where
    empty = Nothing
    size Nothing = 0
    size (Just a) = 1

    sameSize :: Maybe a -> Maybe a -> Bool
    sameSize x y = size x == size y

instance Size [a] where 
    empty = []
    size = length 
    sameSize x y = size x == size y

class Example a where 
    example :: a
    examples :: [a]
    examples = [example]

instance Example Int where
    example = 1
    examples = [0, 1, 2]

instance Example Bool where
    example = True

-- The Int Pair daTAtype
data IntPair = IntPair Int Int 
    deriving Show

instance Eq IntPair where
    (==) :: IntPair -> IntPair -> Bool
    IntPair a1 b1 == IntPair a2 b2 = a1 == a2  && b1 == b2

instance Ord IntPair where
    (<=):: IntPair -> IntPair -> Bool
    IntPair a1 b1 <= IntPair a2 b2 
        | a1 < a2 = True
        | a1 > a2 = False
        | otherwise = b1 < b2
        

