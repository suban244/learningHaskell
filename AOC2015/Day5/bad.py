"""
containsSubstring' :: [Char] -> [Char] -> [Char] -> Bool
containsSubstring' _ "" _ = True
containsSubstring' "" _ _ = False
containsSubstring' string target matchedSoFar
  | head target == head string =
      containsSubstring' (tail string) (tail target) (matchedSoFar ++ head string : "")
        || (null matchedSoFar && containsSubstring' (tail string) target "")
  | otherwise = containsSubstring' (tail string) target ""

"""


def func(string, target, matchedSoFar):
    if target == "":
        return True
    elif string == "":
        return False
    elif target[0] == string[0]:
        return func(string[1:], target[1:], matchedSoFar + string[0]) or (len(matchedSoFar) == 0 and func(string[1:], target, ""))
    else:
        return func(string[1:], target, "")


def func2(string, target, matchedSoFar):
    if target == "":
        return True
    elif string == "":
        return False
    elif target[0] == string[0]:
        return func2(string[1:], target[1:], matchedSoFar + string[0])
    else:
        return func2((matchedSoFar + string)[1:], matchedSoFar + target, "")


