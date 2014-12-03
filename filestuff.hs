import Data.String

highestBin :: Int -> Int
highestBin a = head$drop 0 $ map (\x -> if (a-x) > 0 then 0 else x) (iterate (*2) 1)

asdf :: Int -> Int
asdf a = round$(255/((highestBin a)-1))*(fromIntegral$a-(highestBin a))

main = do
    dragon <- readFile "dragon2.ppm"
    putStr$unlines$map show$map asdf$map (read::String->Int)$words dragon
