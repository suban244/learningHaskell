module Gold where

phi :: Double
phi = (1 + sqrt 5) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1

f x = polynomial (polynomial x)

halve :: Int -> Int
halve x = x `div` 2

circleArea :: Double -> Double
circleArea r = pi * rsquared 
    where 
        pi = 3.141592653589793
        rsquared = r * r

main  = do 
    print(polynomial phi)
    print(f phi)

login :: String -> Maybe String
login "a" = Just "unicorn73"
login "b" = Just "megahacker"
login _           = Nothing