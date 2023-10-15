
import Data.List
applyTo1 f = f 1

addThree :: Int -> Int
addThree x = x + 3

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)


substringOfLength :: Int -> String -> [String]
substringOfLength n string =  map (take 3) (tails string)

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k string = map tail (filter (firstCharC c)  (substringOfLength (k + 1) string))
    where
        firstCharC c str = head str == c

whatFollows' :: Char -> Int -> String -> [String]
whatFollows' c k = map tail . filter (\x -> head x == c)  . map (take (k+ 1)) . tails

substring 

