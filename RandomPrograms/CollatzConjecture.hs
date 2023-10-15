step :: Integer -> Integer
step x = if even x then evenStep else oddStep
    where
        evenStep = x `div` 2
        oddStep = 3 * x + 1


collatz :: Integer -> Integer
collatz 1 = 0
collatz x = 1 + collatz (step x)


longest = longest' 0 0

longest' :: Integer -> Integer -> Integer -> Integer
longest' number _ 0 = number

longest' number maxLen x =
    if len > maxLen then
        longest' x len (x - 1)
    else
        longest' number maxLen (x - 1)
    where
        len = collatz x
