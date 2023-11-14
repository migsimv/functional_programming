import Data.List
import Data.Char (toUpper, isLetter)

-- 1a
average :: [Float] -> Float
average [] = 0.0
average xs = sum xs / fromIntegral (length xs)

--2
divides :: Integer -> [Integer]
divides n = dividesRecursive (abs n) 1 []

dividesRecursive :: Integer -> Integer -> [Integer] -> [Integer]
dividesRecursive n x xs
    | x > n = xs
    | n `mod` x == 0 = dividesRecursive n (x + 1) (xs ++ [x])
    | otherwise = dividesRecursive n (x + 1) xs

dividesListComprehension :: Integer -> [Integer]
dividesListComprehension n = [x | x <- [1..(abs n)], (abs n) `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n = divides n == [1, n]

--3
prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

substring :: String -> String -> Bool
substring [] _ = True
substring _ [] = False
substring str (y:ys) = prefix str (y:ys) || substring str ys

--4
permut :: [Integer] -> [Integer] -> Bool
permut xs ys = sort xs == sort ys

--5 
capitalise :: String -> String
capitalise str = [toUpper c | c <- str, isLetter c]

--6 
itemTotal :: [(String, Float)] -> [(String, Float)]
itemTotal items = itemTotalInner items items [] []

getItemPriceSum :: String -> [(String, Float)] -> Float
getItemPriceSum name items = sum [price | (itemName, price) <- items, itemName == name]

itemTotalInner :: [(String, Float)] -> [(String, Float)] -> [String] -> [(String, Float)] -> [(String, Float)]
itemTotalInner [] _ _ total = total
itemTotalInner ((itemName, _):rest) immutableList visited total
    | itemName `elem` visited && null rest = total
    | itemName `elem` visited = itemTotalInner rest immutableList visited total
    | null rest = newItem : total
    | otherwise = itemTotalInner rest immutableList (itemName : visited) (newItem : total)
  where
    newItem = (itemName, getItemPriceSum itemName immutableList)

itemDiscount :: String -> Integer -> [(String, Float)] -> [(String, Float)]
itemDiscount name discountPercentage items = itemDiscountInner name discountPercentage items []

itemDiscountInner :: String -> Integer -> [(String, Float)] -> [(String, Float)] -> [(String, Float)]
itemDiscountInner _ _ [] newItems = reverse newItems
itemDiscountInner name discountPercentage ((itemName, price):rest) newItems
    | itemName == name = itemDiscountInner name discountPercentage rest ((itemName, discountedPrice) : newItems)
    | otherwise = itemDiscountInner name discountPercentage rest ((itemName, price) : newItems)
  where
    discountedPrice = price - (price * fromIntegral discountPercentage / 100.0)


main :: IO ()
main = do
    -- 1
    putStrLn "Testing average:"
    putStrLn "[1.0, 6.0, 4.0, 5.0] average is: "
    print (average [1.0, 6.0, 4.0, 5.0])

    -- 2
    let testCases = [2, 7, 10, 15, 23]

    putStrLn "Testing isPrime function:"
    mapM_ (\n -> putStrLn $ show n ++ " is prime: " ++ show (isPrime n)) testCases

    putStrLn "\nDivisors for test cases (recursive):"
    mapM_ (\n -> putStrLn $ "Divisors of " ++ show n ++ ": " ++ show (divides n)) testCases

    putStrLn "\nDivisors for test cases (list comprehension):"
    mapM_ (\n -> putStrLn $ "Divisors of " ++ show n ++ ": " ++ show (dividesListComprehension n)) testCases

    --3 
    putStrLn "Testing prefix function:"
    putStrLn $ "Is 'abc' a prefix of 'abcdef'? " ++ show (prefix "abc" "abcdef")  -- Should be True
    putStrLn $ "Is 'abc' a prefix of 'xyz'? " ++ show (prefix "abc" "xyz")        -- Should be False
    putStrLn $ "Is 'hello' a prefix of 'helloworld'? " ++ show (prefix "hello" "helloworld")  -- Should be True
    putStrLn $ "Is 'world' a prefix of 'helloworld'? " ++ show (prefix "world" "helloworld")  -- Should be False

    putStrLn "\nTesting substring function:"
    putStrLn $ "Is 'abc' a substring of 'abcdef'? " ++ show (substring "abc" "abcdef")
    putStrLn $ "Is 'abc' a substring of 'xyz'? " ++ show (substring "abc" "xyz")
    putStrLn $ "Is 'hello' a substring of 'helloworld'? " ++ show (substring "hello" "helloworld")
    putStrLn $ "Is 'world' a substring of 'helloworld'? " ++ show (substring "world" "helloworld")

    --4
    let list1 = [1, 2, 3, 4]
    let list2 = [4, 3, 2, 1]
    let list3 = [1, 2, 2, 3, 4]

    putStrLn "Testing permut function:"
    putStrLn $ show list1 ++ " and " ++ show list2 ++ " are permutations: " ++ show (permut list1 list2)
    putStrLn $ show list1 ++ " and " ++ show list3 ++ " are permutations: " ++ show (permut list1 list3)

    --5 
    let inputStr = "Hello, World!123"

    putStrLn "Testing capitalise function:"
    putStrLn $ "Original: " ++ inputStr
    putStrLn $ "Capitalized: " ++ capitalise inputStr


    --6
    let basket1 = [("Apple", 2.0), ("Banana", 1.5), ("Apple", 3.0), ("Cherry", 2.0)]
    let basket2 = [("Orange", 1.0), ("Orange", 1.0), ("Banana", 1.5)]
    
    putStrLn "Testing itemTotal function:"
    putStrLn "Basket 1 (Merged and summed):"
    print (itemTotal basket1)  
    putStrLn "Basket 2 (Merged and summed):"
    print (itemTotal basket2) 

    let basket3 = [("Apple", 2.0), ("Banana", 1.5), ("Apple", 3.0), ("Cherry", 2.0)]
    putStrLn "\nTesting itemDiscount function:"
    putStrLn "Original Basket 3:"
    print basket3
    let discountedBasket = itemDiscount "Apple" 20 basket3
    putStrLn "Basket 3 after 20% discount on 'Apple':"
    print discountedBasket  