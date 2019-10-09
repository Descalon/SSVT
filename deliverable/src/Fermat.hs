-- Exercise 3
-- Test report: use testFermat with argument k to get the lowest number that
-- fools primeTestsF (primeTestsF says it is prime, while in fact it is not).
--
-- Least composite number that fools the check: Running testFermat 5 times:
-- k = 1 yields 39, 33, 21, 15 and 28           average: 27.2
-- k = 2 yields 65, 259, 365, 1729 and 91       average: 501.8
-- k = 3 yields 703, 703, 1105, 15, 2821        average: 1069.4
-- k = 4 yields 8911, 2701, 15841, 1729, 2821   average: 6400.6
-- Higher values of k seems to make primeTestsF more accurate (the average
-- lowest erroneous value seems to get higher when using higher values of k).

module Fermat where
    import Composite
    import ModularExponentiation
    import System.Random

    testFermat :: Int -> IO Integer
    testFermat k = testFermatHelper k 1

    primeTestsF :: Int -> Integer -> IO Bool
    primeTestsF k n = do
     as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
     return (all (\ a -> exM a (n-1) n == 1) as)

    testFermatHelper :: Int -> Integer -> IO Integer
    testFermatHelper k x = do
        b <- primeTestsF k x
        if b == True && x `elem` (take (fromIntegral x) composites') then return x else testFermatHelper k (x + 1)