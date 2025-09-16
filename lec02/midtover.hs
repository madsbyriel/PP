midtover :: [a] -> ([a], [a])
midtover xs = mySplitAt midt xs
    where 
        midt = div (length xs) 2

mySplitAt :: Integral i => i -> [a] -> ([a], [a])
mySplitAt _ [] = ([], [])
mySplitAt 0 xs = ([], xs)
mySplitAt i (x:xs) = let (part1, part2) = mySplitAt (i - 1) xs
                    in (x:part1, part2)

