module SerialRelation where
    type Rel a = [(a,a)]

    isSerial :: Eq a => [a] -> Rel a -> Bool
    isSerial xs r = all (\x -> elem x r') xs
        where
            r' = map (fst) r
            