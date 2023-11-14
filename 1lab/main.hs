import Test.QuickCheck (quickCheck)

-- 1
nAnd1 True x = not x
-- nAnd1 x T rue = not x
nAnd1 False _ = True

nAnd2 x y = not ( x && y)

nAnd3 True True = False
nAnd3 _ _ = True
-- nAnd3 True False = True
-- nAnd3 False True = True
-- nAnd3 False False = True

-- 2
-- nAnd4 :: Bool -> Bool -> Bool
-- nAnd4 x y = not x || not y

prop_nAnd1 :: Bool -> Bool -> Bool
prop_nAnd1 x y = (nAnd2 x y ==  nAnd1 x y) && (nAnd2 x y == nAnd3 x y)

prop_nAnd2 x y
    | not x || not y = nAnd2 x y
    | otherwise = True

-- 3

nDigits :: Integer -> Int
nDigits x
    | x >= 0 = length (show x)
    | x < 0 = nDigits (abs x)

-- 4

nRoots :: Float -> Float -> Float-> Int
nRoots a b c
    | a == 0 = error "the first argument must be non-zero!"
    | (b^2) >  d = 2
    | (b^2) == d = 1
    | (b^2) <  d = 0
    where d = 4.0 * a * c

-- 5

getRoots :: Float -> Float -> Float -> (Float, Float)
getRoots a b c
    | nRoots a b c == 2 && a > 0 = (smallerRoot a b c, largerRoot a b c)
    | nRoots a b c == 2 && a < 0 = (largerRoot a b c, smallerRoot a b c)
    | nRoots a b c == 1 = (smallerRoot a b c, smallerRoot a b c)
    | nRoots a b c == 0 = error "no roots"

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = (-b - sqrt (b^2 - 4 * a * c))/2 * a

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = (-b + sqrt (b^2 - 4 * a * c))/2 * a


SmallerRoot a b c = fst(getRoots a b c) 
LargerRoot a b c = snd(getRoots a b c) 
 
-- 6

power2 :: Integer -> Integer
power2 n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = 2 * power2 (n - 1)

--7

mult :: Integer -> Integer -> Integer
mult m n
    | m == 0 || n == 0 = 0
    | m < 0 && n < 0 = mult (-m) (-n)
    | m < 0 = negate (mult (-m) n)
    | n < 0 = negate (mult m (-n))
    | otherwise = m + mult m (n - 1)

prop_mult m n = mult m n == m * n
--8

prod :: Integer -> Integer -> Integer
prod m n
    |m < n = m * prod (m+1) n
    |m > n = error "Invalid range"
    |m == n = n


fac :: Integer -> Integer
fac 0 = 1
fac n = prod 1 n

main :: IO ()
main = do
    putStrLn "Main"
    -- 1
    putStrLn "Testing nAnd1:"
    putStrLn "nAnd1 True True: "
    print (nAnd1 True True)

    putStrLn "nAnd1 True False: "
    print (nAnd1 True False)

    putStrLn "nAnd1 False False: "
    print (nAnd1 False False)

    putStrLn "Testing nAnd2:"
    putStrLn "nAnd2 True True: "
    print (nAnd2 True True)

    putStrLn "nAnd2 True False: "
    print (nAnd2 True False)

    putStrLn "nAnd2 False False: "
    print (nAnd2 False False)

    putStrLn "Testing nAnd3:"
    putStrLn "nAnd2 True True: "
    print (nAnd3 True True)

    putStrLn "nAnd2 True False: "
    print (nAnd3 True False)

    putStrLn "nAnd3 False False: "
    print (nAnd3 False False)
    -- 2
    putStrLn "Testing prop_nAnd1"
    quickCheck prop_nAnd1

    putStrLn "Testing prop_nAnd2"
    quickCheck prop_nAnd2
    --3
    putStrLn "Testing nDigits:"

    putStrLn "Number of digits in 12345:"
    print (nDigits 12345)

    putStrLn "Number of digits in -987654321:"
    print (nDigits (-987654321))

    putStrLn "Number of digits in 0:"
    print (nDigits 0)
    --4
    putStrLn "Testing nRoots:"

    putStrLn "a*x^2 + b*x + c = 0 with a=1.0, b=-5.0, c=6.0"
    print (nRoots 1.0 (-5.0) 6.0)

    putStrLn "a*x^2 + b*x + c = 0 with a=2.0, b=4.0, c=2.0"
    print (nRoots 2.0 4.0 2.0)

    -- putStrLn "a*x^2 + b*x + c = 0 with a=0.0, b=2.0, c=1.0"
    -- print (nRoots 0.0 2.0 1.0)

    --5
    print ("testing root 1:", smallerRoot 1 (-5) 6)
    print ("testing root 2:", largerRoot 1 (-5) 6)

    --6 
    putStrLn "Testing power2:"

    putStrLn "Testing 2^0:"
    print (power2 0)

    putStrLn "Testing 2^5:"
    print (power2 5)

    putStrLn "Testing 2^-3:"
    print (power2 (-3))
    --7
    putStrLn "Testing mult:"

    putStrLn "Testing 5 * 3:"
    print (mult 5 3)

    putStrLn "Testing (-4) * 6:"
    print (mult (-4) 6)

    putStrLn "Testing 2 * (-8):"
    print (mult 2 (-8))

    putStrLn "Testing (-3) * (-7):"
    print (mult (-3) (-7))

    putStrLn "Testing 0 * 9:"
    print (mult 0 9)
    --8
    putStrLn "Testing fac:"
    print (fac 5)

    putStrLn "Testing 1 and 3:"
    print (prod 1 3)

    -- putStrLn "Testing 4 and 2:"
    -- print (prod 4 2)
    -- putStrLn "Testing prop_nAnd2"
    quickCheck prop_mult