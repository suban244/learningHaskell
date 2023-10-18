import qualified Data.Map as Map

ageList = [("suban", 21), ("notsuban", 22), ("monke", 30)]

withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank = 
    case Map.lookup account bank of 
        Nothing -> bank
        Just balance -> Map.insert account (balance - amount) bank

withdraw' :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw' account amount bank = Map.adjust (\x -> x - amount) account bank

main = do 
    let ageMap = Map.fromList ageList
    let newageMap = Map.insert "doggo" 7 ageMap
    print $ Map.lookup "suban" ageMap
    print $ Map.lookup "banana" ageMap


