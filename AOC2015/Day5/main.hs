import Data.List (intersect)

-- not 154
hasDouble' lastChar string
  | null string = False
  | head string == lastChar = True
  | otherwise = hasDouble' (head string) (tail string)

hasDouble = hasDouble' ""

twoCharSet string
  | length string > 1 = take 2 string : twoCharSet (tail string)
  | otherwise = []

hasNoBadString string = null (twoCharSet string `intersect` ["ab", "cd", "pq", "xy"])

-- not a good implementation
-- is wrong
-- hasNoBadString gets the job done
containsSubstring' :: [Char] -> [Char] -> [Char] -> Bool
containsSubstring' _ "" _ = True
containsSubstring' "" _ _ = False
containsSubstring' string target matchedSoFar
  | head target == head string =
      containsSubstring' (tail string) (tail target) (matchedSoFar ++ head string : "")
        || (null matchedSoFar && containsSubstring' (tail string) target "")
  | otherwise = containsSubstring' (tail string) target ""

containsSubString string target = containsSubstring' string target ""

itemIn :: Eq a => a -> [a] -> Bool
itemIn char [] = False
-- itemIn char "" = False
itemIn char targetChars
  | char == head targetChars = True
  | otherwise = itemIn char $ tail targetChars

vowelCount :: String -> Int
vowelCount "" = 0
vowelCount string = val + vowelCount (tail string)
  where
    isVowel = itemIn (head string) "aeiou"
    val
      | isVowel = 1
      | otherwise = 0

containsNotOverLappingPair string =
  containsNonAdjacentEqualItems pairs
  where
    pairs = twoCharSet string

containsNonAdjacentEqualItems pairs
  | length pairs <= 2 = False
  | otherwise = itemIn (head pairs) (drop 2 pairs) || containsNonAdjacentEqualItems (tail pairs)

repeatWithXItemInBetween items x
  | length items < x + 2 = False
  | head items == last (take (x + 2) items) = True
  | otherwise = repeatWithXItemInBetween (tail items) x

-- dones't work
-- isNice string = vowelCount string >= 3 && hasDouble string && not (any (containsSubString string) ["ab", "cd", "pq", "xy"])

-- P1
-- isNice string = vowelCount string >= 3 && hasDouble string && hasNoBadString string

-- P2
-- Repeat huna paro but not repeatwithX iteminbetween string 1
isNice string = containsNotOverLappingPair string && repeatWithXItemInBetween string 1

main = do
  contents <- readFile "input.txt"
  let niceCount = sum (map (fromEnum . isNice) (lines contents))
  print niceCount