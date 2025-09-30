onlytwo :: [a] -> Bool
onlytwo [x, y] = True
onlytwo _ = False

alldots :: Num a => [(a, a)] -> [(a, a)] -> [a]
alldots xs ys = [a * c + b * d | (a, b) <- xs, (c, d) <- ys]

alldotsTests = testExecutor alldotsTestCases

alldotsTestCases :: Num a => [(([(a, a)], [(a, a)]), [a])]
alldotsTestCases = [alldotsCase1, alldotsCase2, alldotsCase3, alldotsCase4]

alldotsCase1 :: Num a => (([(a, a)], [(a, a)]), [a])
alldotsCase1 = (([], []), [])

alldotsCase2 :: Num a => (([(a, a)], [(a, a)]), [a])
alldotsCase2 = (([(1, 1)], [(1, 1)]), [2])

alldotsCase3 :: Num a => (([(a, a)], [(a, a)]), [a])
alldotsCase3 = (([(3, 4)], [(1, 2)]), [3 + 4 * 2])

alldotsCase4 :: Num a => (([(a, a)], [(a, a)]), [a])
alldotsCase4 = (([(3, 4), (5, 6)], [(1, 2)]), [11, 17])

testExecutor [] = Nothing
testExecutor (((l1, l2), result):xs)
    | alldots l1 l2 == result = testExecutor xs
    | otherwise = Just (concat ["case: ", show l1, ", ", show l2, ", got: ", show (alldots l1 l2), ", expected: ", show result])

idhead :: Eq a => [(a, a)] -> Bool
idhead ((a,b):xs) = a == b



-- -----------------------------------

pyth :: Int -> [(Int,Int,Int)]
pyth x = [(a, b, c) |  a <- [1..x], b <- [1..x], c <- [1..x], a <= b, b < c, a * a + b * b == c * c]

bighead :: Ord a => [a] -> Int
bighead xs = length [x | x <- xs, x > head xs]

plonk x y z = x + y + z

plonkAlt = \x -> (\y -> (\z -> x + y + z))

isPerfect :: Int -> Bool
isPerfect x
    | x == sum [x' | x' <- [1..(x - 1)], mod x x' == 0] = True
    | otherwise = False


sums n m = [x + y | (x, y) <- concat [zip (repeat a) b | (a, b) <- zip [1..n] (repeat [1..m])]]






