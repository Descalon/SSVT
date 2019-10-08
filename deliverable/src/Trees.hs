module Trees where
    import Lib
    import Test.QuickCheck
    
    -- Exercise 6
    -- Answers to the questions:
    -- It is possible to test that both sets contain the same elements. We do this/
    -- in the tests below where we construct both sets (for each question) using the properties
    -- described. It is then possible to check whether the elements of the first set are also in the second
    -- and then to check whether the elements of the second set are also in the first.

    -- Question 1
    tree1 n = grow (step1 n) (1,1)
    step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

    -- Question 2
    tree2 n = grow (step2 n) (1,1)
    step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

    -- Tests
    collect :: Tree a -> [a]
    collect (T x ts) = x : concatMap Trees.collect ts

    -- create a set where x and y are coprime
    generateTestSet :: Integer -> [(Integer, Integer)]
    generateTestSet n = [(x, y) | x <- [1..n], y <- [1..n], coprime x y]

    -- quickCheck 'helper' function to test tree1 in comparison to
    -- generateTestSet
    coprimeTest1 :: Integer -> Bool
    coprimeTest1 n = length x == length y && setContainsSet x y
            where   x = (Trees.collect $ tree1 n)
                    y = (generateTestSet n)

    -- quickCheck 'helper' function to test tree2 in comparison to
    -- generateTestSet
    coprimeTest2 :: Integer -> Bool
    coprimeTest2 n = length x == length y && setContainsSet x y
            where   x = (Trees.collect $ tree2 n)
                    y = (generateTestSet n)

    -- compare sets iteratively, making sure all combinations in xs are found
    -- in ys
    setContainsSet :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
    setContainsSet [] [] = True
    setContainsSet (x:[]) (ys) = x `elem` ys
    setContainsSet (x:xs) (ys) = x `elem` ys && setContainsSet xs ys

    -- Test Report
    -- Question 1 test return:
    -- quickCheck $ forAll genPos $ coprimeTest1
    --  +++ OK, passed 100 tests.
    -- Question 2 test return:
    -- quickCheck $ forAll genPos $ coprimeTest2
    --  +++ OK, passed 100 tests.
