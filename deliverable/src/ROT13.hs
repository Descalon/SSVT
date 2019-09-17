module ROT13 where
    import Lib
    import Data.Char
    import Data.List
    {--
        Spec: 
        ROTx is a encryption method where all letters of the alphabet are shifted by x spaces. E.G. ROT13 is where a letter is shifted 13 spaces. A becomes N, B becomes O, etc. 
    --}

    numToAlpha i = lookup (i `mod` 26) $ zip [0 .. 25] ['A' .. 'Z']
    alphaToNum c = lookup c $ zip ['A' .. 'Z'] [0 .. 25]

    rotx :: Int -> Char -> Char
    rotx i c = 
            let index = case alphaToNum c of    Nothing -> error ("Can't find char " ++ show c)
                                                Just x -> x
            in case numToAlpha (index + i) of   Nothing -> error "This shouldn't happen"
                                                Just x -> x
    rotxString i = map (rotx i)
    rot13 = rotx 13

    rot13String = map (rot13) 

    mapper :: [Char] -> [Bool]
    mapper = map (isUpper)
    
    folder :: [Bool] -> Bool
    folder = foldr (&&) True

    precondition :: [Char] -> Bool
    precondition = folder . mapper

    prop_same_length :: [Char] -> Bool
    prop_same_length input = precondition input --> postcondition (rot13String input)
                        where postcondition x = (length x) == (length input) 

    prop_reversable :: [Char] -> Bool
    prop_reversable input = precondition input --> postcondition (rot13String input)
                        where postcondition x = (rotxString (-13) x == input)