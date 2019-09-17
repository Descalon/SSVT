{--
    Ran with ghci 5 times:
    Test results:
    [0.2511,0.2377,0.2353,0.2476]
    [0.2518,0.2413,0.2374,0.2374]
    [0.2513,0.2430,0.2382,0.2340]
    [0.2576,0.2327,0.2400,0.2418]
    
    Results well within range of 25% per quartile
--}

module Quartiles where
    import Lib

    quartiles :: [(Float, Float)]
    quartiles = [ 
        (0.00, 0.25),
        (0.26, 0.50),
        (0.51, 0.75),
        (0.76, 1.00)
        ]

    inRange :: (Float, Float) -> Float -> Bool
    inRange (min, max) v = min <= v && max >= v
    
    percentile :: (Float, Float) -> [Float] -> Float
    percentile r xs = lf' / l'
        where
            inRange' = inRange r
            f' = filter (inRange') xs
            l' = fromIntegral . length $ xs
            lf' = fromIntegral . length $ f'
    pq :: [[Float] -> Float]
    pq = map (percentile) quartiles
    
    calc :: [Float] -> [Float]
    calc xs = map (\ y -> y xs) pq
    
    calcM :: Int -> IO [Float]
    calcM n = do
            xs <- probs n
            return (calc xs)