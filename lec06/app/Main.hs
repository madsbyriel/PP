module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

sumsq :: Int -> Integer
sumsq = foldr ((+) . (^ 2)) 0 . flip take [1..]

toPosition :: Char -> Int
toPosition = flip (-) 96 . fromEnum

positions = map toPosition 

--------------------------------
-- In class

dbs = filter (\(first, second) -> first * 2 == second)

within xs (bot, top) = filter (\x -> (bot <= x) && (top >= x)) xs

sumrows :: Num a => [[a]] -> [a]
sumrows = map sum

approx' = foldr (\a acc -> (1 / fact a) + acc) 0 . flip take [0..]
approx = sum . map ((/) 1 . fact) . flip take [0..]
fact = product . flip take [1..]

--------------------------------

-- a
partitionFoldr :: (a -> Bool) -> [a] -> ([a], [a])
partitionFoldr p = foldr (\a (t, f) -> if p a then (a:t, f) else (t, a:f)) ([], [])

partitionFilter :: (a -> Bool) -> [a] -> ([a], [a])
partitionFilter p xs = (filter p xs, filter (not . p) xs)

-- b
filter' p = foldr (\a b -> if p a then a:b else b) []

-- c
remove' :: String -> String -> String
remove' xs = foldr (\a b -> if a `elem` xs then b else a:b) []

isIn :: Eq a => a -> [a] -> Bool
isIn _ [] = False
isIn y (x:xs)
    | y == x = True
    | otherwise = isIn y xs

-- d
-- map er en function over en liste der udregner en ny værdi pr. type. Hvis map kan anvende map på ethvert element, så må typen a være en liste af den vilkårlige type 'c' der mapper til den vilkårlige type 'd'. Derved må map map være:
-- (c -> d) -> [[c]] -> [[d]]
-- det var så ikke rigtigt. den korrekte type er [a -> b] -> [[a] -> [b]]
-- hvilket jo er en liste af funktioner fra elementet a til elementet b der returnerer en liste af funktioner fra lister af type a til lister af type b
-- hvilket giver mening...

tes :: [a -> b] -> [[a] -> [b]]
tes = map map

-- e 
-- min2 = 
