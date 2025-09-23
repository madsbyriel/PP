module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"



twice :: (a -> a) -> a -> a
twice f = f . f



thesame :: Eq b => [(b, b)] -> [(b, b)]
thesame xs = [(a, b) | (a, b) <- xs, a == b]


