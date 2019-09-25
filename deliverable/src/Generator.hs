module Generator where
    import SetOrd
    import System.Random
    import Test.QuickCheck

    generateInts :: IO [Int]
    generateInts = do
        g <- newStdGen
        return (randomRs (0,9) g :: [Int])

    generateSet :: Int -> IO (Set Int)
    generateSet count  = do
        rs <- generateInts 
        let rs' = take count rs
        return (Set rs')

    generateSet' :: Int -> Gen (Set Int)
    generateSet' size = do
        k <- choose (0, size)
        l <- sequence [arbitrary | _ <- [1..k]]
        return (Set l)
    
    sizedSet = sized generateSet'