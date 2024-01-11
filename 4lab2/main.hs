-- module Main where
-- import qualified Data.Set as Set
-- import GHC.Data.BooleanFormula ( eval )
import Data.Maybe (fromJust)

--1
data GTree a = Leaf a | Gnode [GTree a]
    deriving (Show)

treeDepth :: GTree a -> Integer
treeDepth (Leaf _) = 0
treeDepth (Gnode xs) = 1 + maximum [treeDepth subtree | subtree <- xs] 

treeOccurs :: Eq a => GTree a -> a -> Bool
treeOccurs (Leaf x) y = x == y
treeOccurs (Gnode xs) y = or [treeOccurs subtree y | subtree <- xs]

mapTree :: (a -> b) -> GTree a -> GTree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Gnode xs) = Gnode [mapTree f subtree | subtree <- xs]

--2
data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a] 
type Ops a = [a] -> a
type Var = Char
type Valuation a = [(Var, a)]

eval :: Valuation a -> Expr a -> a
eval _ (Lit x) = x
eval vals (EVar var) = fromJust $ lookup var vals
eval vals (Op f exprs) = f $ map (eval vals) exprs

expression :: Expr Integer
expression = Op sum [Op product [EVar 'a', EVar 'b'], Lit 2]

valuation :: Valuation Integer
valuation = [('a', 5), ('b', 3), ('c', 2)]

--3
type RegExp = String -> Bool

-- matchina empty string
epsilon :: RegExp
epsilon = (== "")

-- single char
char :: Char -> RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
p1 ||| p2 = \x -> p1 x || p2 x

(<*>) :: RegExp -> RegExp -> RegExp
p1 <*> p2 = \x ->
  or [p1 y && p2 z | (y, z) <- splits x]

--visi imanomi splitai
splits :: [a] -> [([a], [a])]
splits xs = [splitAt x xs | x <- [0..(length xs)]]

star :: RegExp -> RegExp
star p = epsilon ||| (p Main.<*> star p)

option :: RegExp -> RegExp
option p = epsilon ||| p

plus :: RegExp -> RegExp
plus p = p Main.<*> star p

--4
data NumList a = Nlist [a]

instance (Fractional a, Eq a) => Eq (NumList a) where
    (==) :: (Fractional a, Eq a) => NumList a -> NumList a -> Bool
    Nlist xs == Nlist ys = average xs == average ys

instance (Fractional a, Ord a) => Ord (NumList a) where
    compare (Nlist xs) (Nlist ys) = compare (average xs) (average ys)

average :: Fractional a => [a] -> a
average [] = 0
average xs = sum xs / fromIntegral (length xs)

--5
data Result a = OK a | Error String

composeResult :: (a -> Result b) -> (b -> Result c) -> (a -> Result c)
composeResult f g = \x ->
    case f x of
        OK y -> g y
        Error err -> Error err

--6 
primes :: [Integer]
primes = sieve [2..]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

goldbach :: Integer -> Bool
goldbach n = all (`elem` sums) evens
 where
    evens = [x | x <- [4..n], even x]
    primesNum = takeWhile (<= n) primes
    sums = [x+y | x <- primesNum, y <- primesNum]
--7

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)







main :: IO ()
main = do
    --1 
    -- let sampleTree = Gnode [Leaf 1, Gnode [Leaf 2, Leaf 3], Gnode [Gnode [Leaf 4, Leaf 5], Leaf 6]]

--     print sampleTree

--     putStrLn $ "Tree Depth: " ++ show (treeDepth sampleTree)

--     putStrLn $ "Tree Occurs:" ++ show (treeOccurs sampleTree 3) ++ show (treeOccurs sampleTree 7)

    putStrLn "Mapped Tree:"
--     print $ mapTree (* 2) sampleTree

-- 3
    -- print $ eval epsilon ""               -- epsilon matches empty string
    -- print $ eval epsilon ""               -- epsilon matches empty string
    -- print $ eval (char 'a') "a"           -- char 'a' matches 'a'
    -- print $ eval (char 'a') "b"           -- char 'a' does not match 'b'
    -- print $ eval (char 'a' ||| char 'b') "a"  -- char 'a' ||| char 'b' matches 'a'
    -- print $ eval (char 'a' <*> char 'b') "ab" -- char 'a' <*> char 'b' matches 'ab'
    -- print $ eval (star (char 'a')) "aaaa"     -- star (char 'a') matches 'aaaa'
--     -- print $ eval (option (char 'a')) "b"      -- option (char 'a') does not match 'b'
--     -- print $ eval (plus (char 'a')) "aaaa"     -- plus (char 'a') matches 'aaaa'
-- -- --4 
--     let list1 = Nlist [1.0, 2.0, 3.0]
--         list2 = Nlist [1.0, 2.0, 3.0]
--         list3 = Nlist [4.0, 5.0, 6.0]

--     print $ list1 == list2  -- Expect: True
--     print $ list1 == list3  -- Expect: False

--     print $ compare list1 list2  -- Expect: EQ
--     print $ compare list1 list3  -- Expect: LT
--     print $ compare list3 list1  -- Expect: GT
-- --6
--     -- let upperBound = 100
--     -- putStrLn "Goldbach Conjecture Check:"
--     -- putStrLn $ "Is Goldbach conjecture true up to " ++ show upperBound ++ "? " ++ show (goldbach upperBound)


--     -- --7
--     -- putStrLn "Stream Examples:"

--     -- putStrLn "Nats Stream:"
--     -- print $ take 5 $ streamToList (nats 1)

--     -- putStrLn "Praise Stream:"
--     -- print $ take 5 $ streamToList praise

--     -- putStrLn "Religious Figures Stream:"
--     -- print $ take 5 $ streamToList (religiousFigures 1)