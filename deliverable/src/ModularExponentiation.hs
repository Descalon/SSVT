module ModularExponentiation where
    import Lib
    import Test.QuickCheck
    import Test.QuickCheck.Monadic
    import Data.Bits
    import Text.Printf
    import System.CPUTime

    (%) :: Integer -> Integer -> Integer
    a % b = a `mod` b

    slowExM :: Integer -> Integer -> Integer -> Integer
    slowExM x e m = (x ^ e) % m

    exM :: Integer -> Integer -> Integer -> Integer
    exM x e m = 
        let f c e' = if e' < e then f c' (e'+1) else c'
                where c' = (x * c) % m
        in f 1 1

    getBitSize :: Integer -> Int
    getBitSize x = floor $ logBase 2 $ fromIntegral x

    collectSquares :: Integer -> Integer -> Integer -> [Integer]
    collectSquares x e m = 
        let f x' e' = if e' < e then c : f c (e' * 2) else [c]
                where c = (x' * x') % m
            first = x % m
        in first : (f x 2)

    selectSquares :: [Integer] -> Integer -> Int -> [Integer]
    selectSquares _ _ (-1) = []
    selectSquares [] _ _ = []
    selectSquares (sq:sqs) x b = if testBit x b then
                                    sq : selectSquares sqs x (b-1)
                                 else
                                    selectSquares sqs x (b-1)

    --exM' :: Integer -> Integer -> Integer -> Integer
    exM' x e m = x' % m
        where 
            l = floor $ logBase 2 (fromIntegral e) 
            sqs = reverse $ collectSquares x (2^l) m
            x' = product $ selectSquares sqs e l
    
    clamp = 0
    genPos :: Gen (Integer, Integer, Integer)
    genPos = (arbitrary :: Gen (Integer,Integer,Integer)) `suchThat` (\ (x,y,z) -> x > clamp && y > clamp && z > clamp)

    genList :: Gen [(Integer, Integer, Integer)]
    genList = listOf genPos

    wrapper f (x,e,m) = (f x e m) >= 0
        
    diffNs :: Integer -> Integer -> Float
    diffNs s e = (fromIntegral (e - s))

    testPerformance :: (Integer, Integer, Integer) -> (Integer -> Integer -> Integer -> Integer) -> IO (Integer, Float)
    testPerformance (x, e, m) f = do
        s <- getCPUTime
        printf "started on %i\n" s
        let r = f x e m
        e <- getCPUTime
        printf "ended on %i\n" e

        printf "result is %i\n" r
        let d = diffNs s e
        return (r,d)

    printResult :: String -> (Integer, Float) -> IO ()
    printResult s (r,d) = do
        printf s
        printf "\nRan for %0.3f ns\n" d
        return ()
    
    testPerformances :: (Integer, Integer, Integer) -> IO Bool
    testPerformances t = do
        m1@(r1,d1) <- testPerformance t slowExM
        printResult "slowExM" m1
        m2@(r2,d2) <- testPerformance t exM
        printResult "exM" m2
        m3@(r3,d3) <- testPerformance t exM'
        printResult "exM2" m3
        return (r1 == r2 && r2 == r3)

    prop_x t = monadicIO $ do
        result <- run (testPerformances t)
        assert $ result