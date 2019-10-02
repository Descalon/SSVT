{-- Exercise 1
    Time spent: ~25 min

--}

module Generator where
    import SetOrd
    import System.Random
    import Test.QuickCheck

    has :: (Eq a) => [a] -> a -> Bool
    has = flip elem

    unique :: (Eq a) => [a] -> [a]
    unique [] = []
    unique (x:xs)
        | has xs x = unique xs
        | otherwise = x : unique xs

    generateInts :: IO [Int]
    generateInts = do
        g <- newStdGen
        return (randomRs (0,9) g :: [Int])

    generateSet :: Int -> IO (Set Int)
    generateSet count  = do
        rs <- generateInts 
        let rs' = unique $ take count rs
        return (Set rs')

    generateSet' :: Int -> Gen (Set Int)
    generateSet' size = do
        k <- choose (0, size)
        l <- sequence [arbitrary | _ <- [1..k]]
        return (Set $ unique l)
    
    sizedSet = sized generateSet'