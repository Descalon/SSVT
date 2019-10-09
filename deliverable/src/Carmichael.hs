module Carmichael where
    import Lib
    import Fermat
    import ModularExponentiation

    nee = 3

--     mrComposite :: Integer -> Integer -> Bool
--     mrComposite x n = let
--         (r,s) = decomp (n-1)
--         fs     = takeWhile (/= 1)
--            (map (\ j -> exM x (2^j*s) n)  [0..r])
--       in
--         exM x s n /= 1 && last fs /= (n-1)
    
--     primeMR :: Int -> Integer -> IO Bool
--     primeMR _ 2 = return True
--     primeMR 0 _ = return True
--     primeMR k n = do
--         a <- randomRIO (2, n-1) :: IO Integer
--         if exM a (n-1) n /= 1 || mrComposite a n
--         then return False else primeMR (k-1) n

--     -- Exercise 4
--     carmichael :: [Integer]
--     carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
--         k <- [2..],
--         prime (6*k+1),
--         prime (12*k+1),
--         prime (18*k+1) ]

--     -- Answers to the questions
--     -- 4.1
--     -- We find that that testFermatHelper tends return the head of the carmichael lists, even though it is a composite.
--     -- 4.2
--     -- Given Carmichael numbers share similar characteristics to the characteristics
--     -- of the little Ferment theorem, the little Ferment theorem fails on every
--     -- carmichael number by stating it would in fact be a prime, where it's not.
--     -- Carmichael numbers are always composite. The characteristic; if pc is a
--     -- prime OR a carmichael number, then for any int b; b ^ pc - b is a multiple of pc.
--     -- 4.3
--     -- We find that the testPrimeMR never returns a value. This is due to the fact that
--     -- primeMR always returns false.
--     -- Tests
--     -- 4.1
--     -- Performse testFermatHelper with carmichael numbers.
--     testFermatHelperCarmichael k = testFermatHelper carmichael k
--     -- 4.3
--     -- Tests the primeMR with an array of integers by choise. 
--     testPrimeMR :: Int -> [Integer] -> IO()
--     testPrimeMR _ []    = print("Finished Test")
--     testPrimeMR k (x:xs)= do
--                         result <- primeMR k x
--                         if (result == True) then print(x) else
--                             testPrimeMR k xs
