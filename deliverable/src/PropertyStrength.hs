{-- Exercise 2
    Results: [("prop1",3),("prop4",0),("prop3",0),("prop2",-3)]
    Ran with ghci calling the sorted value.    
--}
module PropertyStrength where
    import Data.List
    import Lib

    prop1,prop2,prop3,prop4 :: Int -> Bool
    prop1 = (\ x -> even x && x > 3)
    prop2 = (\ x -> even x || x > 3)
    prop3 = (\ x -> (even x && x > 3) || even x)
    prop4 = even

    props = [prop1,prop2,prop3,prop4]
    propNames = ["prop1","prop2","prop3","prop4"]
    
    domain :: [Int]
    domain = [-10 .. 10]

    stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
    stronger xs p q = forall xs (\ x -> p x --> q x)
    weaker   xs p q = stronger xs q p 

    compare' :: [a] -> (a -> Bool) -> (a -> Bool) -> Int
    compare' xs p q = 
            let s = stronger xs p q
                w = weaker xs p q
            in if s == w then 0 else if s then 1 else -1
    
    score :: [a] -> (a -> Bool) -> [(a -> Bool)] -> Int
    score xs p q = sum $ map (\x -> compare' xs p x) q

    scoreAll :: [a] -> [(a -> Bool)] -> [Int]
    scoreAll input target = map (\x -> score input x target) target

    sort' [] = []
    sort' (x:xs) = 
            let value = snd x
                smaller = filter (\y -> snd y < value) xs
                larger = filter (\y -> snd y >= value) xs
            in (sort' larger) ++ [x] ++ (sort' smaller)

    scoreWithNames :: [(String,Int)]
    scoreWithNames = zip propNames $ scoreAll domain props

    sorted = sort' scoreWithNames