-- Exercise 2
-- Test report: using compositesTest checks whether results from composites are
-- not prime. Uses the prime function from Lecture5 module.
--
-- quickCheck compositesTest yield the following results:
--  +++ OK, passed 100 tests.
--
-- CompositesTest with values 500, 5000, 10000, 20000 and 100000 also all
-- yield True, so implementation looks correct.

-- Return list of composite integers

module Composite where
    import Lib

    composites' :: [Integer]
    composites' = [x | x <- [1..], any (\y -> x `mod` y == 0) [2..(x - 1)]]

    -- Checks whether first n results from composites function are not prime
    compositesTest :: Int -> Bool
    compositesTest n = all (\x -> (not . prime) x) (take n  composites')
