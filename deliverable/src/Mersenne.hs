-- Exercise 5
-- produceMersenne randomly checks of one of the first 9 primes whether the 
-- result of 2 ^ p - 1 is also prime. If they are also prime, then they are 
-- mersenne primes. This is checked with the website 
-- http://mathworld.wolfram.com/MersennePrime.html, which lists the first few
-- Mersenne primes.
-- Picking a prime above the first 9 makes the program take too much time, so
-- these are not checked.
-- The PrimeIndex variant of the function takes one argument: the index of the
-- in the list of primes produced by the primes function. It then checks whether
-- 2 ^ p - 1 is prime as well.
-- the second function with argument 0 (checking the first prime, 2) yields
-- True, argument 1 (checking the second prime, 3) yields True. Running it with
-- all arguments [0..9] yields 7 times True, 3 times False as results. All
-- cases that yield True produce a mersenne prime (checked with the earlier
-- mentioned website). So for the tested cases, 70% are mersenne primes.
module Mersenne where
    import System.Random
    import Lib
    import ModularExponentiation

    mrComposite :: Integer -> Integer -> Bool
    mrComposite x n = let
        (r,s) = decomp (n-1)
        fs     = takeWhile (/= 1)
           (map (\ j -> exM x (2^j*s) n)  [0..r])
      in
        exM x s n /= 1 && last fs /= (n-1)
    
    primeMR :: Int -> Integer -> IO Bool
    primeMR _ 2 = return True
    primeMR 0 _ = return True
    primeMR k n = do
        a <- randomRIO (2, n-1) :: IO Integer
        if exM a (n-1) n /= 1 || mrComposite a n
        then return False else primeMR (k-1) n

    produceMersenne :: IO Bool
    produceMersenne = do
        r <- randomRIO (0,8)
        putStrLn ("Prime: " ++ (show (primes !! r)))
        putStrLn ("Also prime? : " ++ (show ((2 ^ (primes !! r)) - 1)))
        (primeMR 1 ((2 ^ (primes !! r)) - 1))

    produceMersennePrimeIndex :: Int -> IO Bool
    produceMersennePrimeIndex x = do
        putStrLn ("Prime: " ++ (show (primes !! x)))
        putStrLn ("Also prime? : " ++ (show ((2 ^ (primes !! x)) - 1)))
        (primeMR 1 ((2 ^ (primes !! x)) - 1))
