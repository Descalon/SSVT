{-- Exercise 1
    Main implementation based on material from Khan Academy (https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation)
    Performance tested by using the testPerformances function and enabeling the GHCI statistics (:set +s). The results can vary wildly and given is a list of tests run:

    components (x, e, m) > 1000
    slowExM : 9.984s,  10.094s, 10.828s
    exM     : 10.469s,  9.938s, 10.781s
    exM'    : 10.484s, 10.391s, 10.547s

    comps > 100
    slowExM : 171.875ms, 140.625ms, 109.375ms
    exM     : 125.000ms, 109.375ms, 156.250ms
    exM'    : 140.625ms, 125.000ms, 187.500ms

    
--}

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

    exM' :: Integer -> Integer -> Integer -> Integer
    exM' x e m = x' % m
        where 
            l = floor $ logBase 2 (fromIntegral e) 
            sqs = reverse $ collectSquares x (2^l) m
            x' = product $ selectSquares sqs e l
    
    clamp = 100
    genPos :: Gen (Integer, Integer, Integer)
    genPos = (arbitrary :: Gen (Integer,Integer,Integer)) `suchThat` (\ (x,y,z) -> x > clamp && y > clamp && z > clamp)

    genList :: Gen [(Integer, Integer, Integer)]
    genList = listOf genPos

    wrapper f (x,e,m) = (f x e m) >= 0
        
    diffMs :: Integer -> Integer -> Float
    diffMs s e = (fromIntegral (e - s)) / (10 ^ 9)

    testPerformance :: (Integer -> Integer -> Integer -> Integer) -> IO Float
    testPerformance f = do
        s <- getCPUTime
        quickCheck $ forAll genPos $ \ (x,e,m) -> (f x e m) >= 0
        e <- getCPUTime
        let d = diffMs s e
        return d

    printResult :: String -> Float -> IO ()
    printResult s d = do
        printf s
        printf "\nRan for %0.3f ms\n" d
        return ()
    
    testPerformances :: IO ()
    testPerformances = do
        d1 <- testPerformance slowExM
        printResult "slowExM" d1
        d2 <- testPerformance exM
        printResult "exM" d2
        d3 <- testPerformance exM'
        printResult "exM2" d3
        
        return ()