module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Tree a = Leaf a | Node (Tree a) a (Tree a)
data Unary = I Unary | Z;

---
-- Prep

unary2int :: Unary -> Int
unary2int Z = 0
unary2int (I unary) = 1 + unary2int unary

least :: Ord a => Tree a -> a
least (Leaf a) = a
least (Node t1 a t2) = min (min (least t1) (least t2)) a

---
-- Lecture

data Aexp = N Int | X String | Add Aexp Aexp | Mul Aexp Aexp

assToLookup :: [(String, Int)] -> (String -> Int)
assToLookup xs s = head [x | (c, x) <- xs, c == s]

eval :: Aexp -> [(String, Int)] -> Int
eval e xs = evalAux e (assToLookup xs)

evalAux :: Aexp -> (String -> Int) -> Int
evalAux (N n) _ = n
evalAux (X s) f = f s
evalAux (Add e1 e2) f = evalAux e1 f + evalAux e2 f
evalAux (Mul e1 e2) f = evalAux e1 f + evalAux e2 f

test1 = eval (Add (X "a") (X "a")) [("a", 3)]

data Encyclopedia a = ENode [Encyclopedia a] String a

---
-- Extra
-- a
countLeaves :: Tree a -> Int
countLeaves (Leaf _) = 1
countLeaves (Node t1 _ t2) = countLeaves t1 + countLeaves t2

balanced :: Tree a -> Bool
balanced (Leaf _) = False
balanced (Node t1 _ t2)
    | num1 > num2 = num1 - 1 == num2
    | num1 < num2 = num2 - 1 == num1
    | otherwise = True
    where
        num1 = countLeaves t1
        num2 = countLeaves t2


-- b
data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop

type Subst = (Char, Bool)

unique :: Ord a => [a] -> [a]
unique (x:xs)
    | x `elem` xs = x : unique xs
    | otherwise = unique xs
unique [] = []

evalProp :: Prop -> (Char -> Bool) -> Bool
evalProp (Var c) f = f c
evalProp (Not p) f = not (evalProp p f)
evalProp (And p1 p2) f = evalProp p1 f && evalProp p2 f
evalProp (Or p1 p2) f = evalProp p1 f || evalProp p2 f
evalProp (Imply p1 p2) f = not (evalProp p1 f) || evalProp p2 f
evalProp (Const b) _ = b

findVars :: Prop -> [Char]
findVars (Var c) = [c]
findVars (Not p) = findVars p
findVars (And p1 p2) = unique (findVars p1 ++ findVars p2)
findVars (Or p1 p2) = unique (findVars p1 ++ findVars p2)
findVars (Imply p1 p2) = unique (findVars p1 ++ findVars p2)
findVars (Const _) = []

listsEq :: Eq a => [a] -> [a] -> Bool
listsEq xs ys = all (`elem` ys) xs && all (`elem` xs) ys

generateAllPerms :: [a] -> [[(a, Bool)]]
genrateAllPerms (x:xs) = 

equiv :: Prop -> Prop -> Bool
equiv p1 p2
    | areEqual = [0 | v <- ]
    | otherwise = False
    where
        vars1 = findVars p1
        vars2 = findVars p2
        areEqual = listsEq vars1 vars2
        allPermutations = [k]

