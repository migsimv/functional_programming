import Data.List (inits)

-- 1
-- rectangle width height ir bottom let coordinates, circle radius ir center coordinate
data Shape = Circle Float (Float, Float) | Rectangle Float Float (Float, Float)
    deriving (Show, Ord, Eq)  

overlaps :: Shape -> Shape -> Bool 
overlaps (Circle r1 (x1, y1)) (Circle r2 (x2, y2)) = 
    distanceBetweenPoints (x1, y1) (x2, y2) <= r1 + r2
overlaps (Rectangle h1 w1 (x1, y1)) (Rectangle h2 w2 (x2, y2)) = 
    not (x1 + w1 < x2 || x2 + w2 < x1 || y1 + h1 < y2 || y2 + h2 < y1)
overlaps (Circle r (x, y)) (Rectangle h w (rx, ry)) = 
    let closestX = max x (min rx (x + w))
        closestY = max y (min ry (y + h))
    in distanceBetweenPoints (x, y) (closestX, closestY) <= r
overlaps (Rectangle h w (rx, ry)) (Circle r (x, y)) =  
    overlaps (Circle r (x, y)) (Rectangle h w (rx, ry))

distanceBetweenPoints :: (Float, Float) -> (Float, Float) -> Float
distanceBetweenPoints (x1, y1) (x2, y2) =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2) -- euclido formule

--2 
-- patikrina ar ne null po filtravimo
myAnyFilter :: (a -> Bool) -> [a] -> Bool 
myAnyFilter p xs = not (null (filter p xs))
--palygina originalu length ir atfiltruota length 
myAllFilter :: (a -> Bool) -> [a] -> Bool 
myAllFilter p xs = length xs == length (filter p xs)

-- foldr - it takes the second argument and the last item of the list and applies the function, then it takes the penultimate item from the end and the result, and so on. See scanr for intermediate results. 
-- example Input: foldr (+) 5 [1,2,3,4] Output: 15
-- foldr ima po elementa is desines
-- lambda funkcija apllyina predicata kiekvienam elementui 
-- lambda funkcija ima x - elementa is listo, acc - turima rezultata so far
-- foldr foldr (+) 5 [1,2,3,4] > 4 + 5 + 3 +2 +1
-- Input: foldr (/) 2 [8,12,24,4] > dalinamas elementas nuo galo, ats 8
myAnyMapFoldr :: (a -> Bool) -> [a] -> Bool  
myAnyMapFoldr p = foldr (\x acc -> p x || acc) False

myAllMapFoldr :: (a -> Bool) -> [a] -> Bool 
myAllMapFoldr p = foldr (\x acc -> p x && acc) True

--3
-- (x,y) - dabartine pora, (accX, accY) - viskas ka gavom iki siol
-- sukonstruoja nauja pora prideda prie accX ir accY
unzipp :: [(a, b)] -> ([a], [b])
unzipp = foldr (\(x, y) (accX, accY) -> (x : accX, y : accY)) ([], [])

-- 4
-- mapas replacina listo elementus i vienetus ir sum 
-- . - compozicijos operatorius
lengthWithMap :: [a] -> Int
lengthWithMap = sum . map (\_ -> 1)

lengthWithFoldr :: [a] -> Int
lengthWithFoldr = foldr (\_ acc -> acc + 1) 0

--5
-- remove negative numbers, multiply each by 10 and sum it up while its < upper bound
-- inits pakuria list listu inits [1, 2, 3] = [[],[1],[1,2],[1,2,3]]
-- taeWhile ima elementus kol tenkina salyga
-- scanl1 (+) [1, 2, 3, 4], result [1, 1+2, (1+2)+3, ((1+2)+3)+4]
ff :: Integer -> [Integer] -> Integer
ff maxNum = sum . takeWhile (\s -> s <= maxNum) . scanl1 (+) . map (* 10) . filter (>= 0)

--6 
total :: (Integer -> Integer) -> Integer -> Integer
total f n = foldr (\x acc -> f x + acc) 0 [0..n]

--7 
-- id - identitiy function, pvz input : id 3, output 3
-- n iteraciju skaicius, f funkcija
iterRec :: Integer -> (a -> a) -> (a -> a)
iterRec n f
  | n <= 0    = id -- stop
  | otherwise = f . iterRec (n - 1) f

--Input: replicate 3 5, Output: [5,5,5]
iterFold :: Integer -> (a -> a) -> (a -> a)
iterFold n f = foldr (.) id (replicate (fromInteger n) f)

--8
splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- splits xs]

main :: IO ()
main = do
    -- 1
    putStrLn $ "Circle 1 and Circle 2: " ++ show (overlaps (Circle 3.0 (1.0, 1.0)) (Circle 2.0 (4.0, 2.0)))
    putStrLn $ "Rectangle 1 and Rectangle 2: " ++ show (overlaps (Rectangle 4.0 3.0 (2.0, 2.0)) (Rectangle 2.0 2.0 (6.0, 1.0)))
    putStrLn $ "Circle 1 and Rectangle 1: " ++ show (overlaps (Circle 3.0 (1.0, 1.0)) (Rectangle 4.0 3.0 (2.0, 2.0)))
    putStrLn $ "Rectangle 2 and Circle 2: " ++ show (overlaps (Rectangle 2.0 2.0 (6.0, 1.0)) (Circle 2.0 (4.0, 2.0)))

    --2
    let numbers = [1, 2, 3, 4, 5]
    print $ myAnyFilter even numbers  -- true
    print $ myAllFilter even numbers  -- false
    print $ myAnyMapFoldr even numbers  -- true
    print $ myAllMapFoldr even numbers  -- Sfalse

    --3 
    print (unzipp [(1, 'a'), (2, 'b'), (3, 'c')])

    --4 
    print $ lengthWithMap [1, 2, 3, 4, 5]  
    print $ lengthWithMap ["apple", "banana", "orange"]  
    print $ lengthWithFoldr  []  

    --5 
    putStrLn "Ex 5. : "
    print (ff 100 [10, -5, 8, 3, -7, 20])  -- Should print: 100
    print ( ff 50 [10, -5, 8, 3, -7, 20])  -- Should print: 0
    print (ff 10 [1, 2, 3, 4, 5])  -- Should print: 10

    --6 
    let square x = x * x
    print ( total square 3)   
    -- 0^2 + 1^2 + 2^2 + 3^2 = 0 + 1 + 4 + 9 = 14

    let identity x = x
    print (total identity 5)  
    --0 + 1 + 2 + 3 + 4 + 5 = 15

    --7 
    let double x = x * 2
    let iterTwice = iterRec 2 double -- double(double x) = (x * 2) * 2
    print $ iterTwice 3  -- 12

    let square x = x * x
    let iterThrice = iterFold 3 square -- square(square (square(x))) = x ^ 8
    print $ iterThrice 2  -- 256

    --8
    print (splits "Spy")
    print (splits [1, 2, 3])
    print (splits "Hello")

