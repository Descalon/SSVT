module SerialRelation where
    import Lib

    type Rel a = [(a,a)]

    isSerial :: Eq a => [a] -> Rel a -> Bool
    isSerial xs r = all (\x -> elem x r') xs
        where
            r' = map (fst) r

    findReflexive :: Eq a => a -> Rel a -> Rel a
    findReflexive x rs = filter (\(f,s) -> x == f && x == s) rs

    isReflexive :: Eq a => [a] -> Rel a -> Bool
    isReflexive domain rs = all (\x -> case findReflexive x rs of [] -> False; _ -> True) domain
            
    reflexiveGenerator :: [a] -> Rel a
    reflexiveGenerator domain = [(x,x) | x <- domain]

    serialGenerator :: [a] -> Rel a
    serialGenerator domain@(d:_) = [(x,d) | x <- domain]

    notSerialGenerator :: [a] -> Rel a
    notSerialGenerator (d:xs) = [(x,d) | x <- xs]

    foo :: Eq a => [a] -> Rel a -> Rel a
    foo xs rs = filter (\x -> (fst x) /= head xs) rs

    prop_when_drop_should_not_be_serial :: Eq a => [a] -> Rel a -> Bool
    prop_when_drop_should_not_be_serial xs rs = isSerial xs rs --> not (isSerial xs rs')
            where   h = head xs
                    rs' = filter (\x -> (fst x) /= h) rs

    prop_reflexive_should_be_serial :: Eq a => [a] -> Rel a -> Bool
    prop_reflexive_should_be_serial xs rs = isReflexive xs rs --> isSerial xs rs