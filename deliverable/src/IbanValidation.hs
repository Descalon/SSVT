{-- Excersise 7
    Validated known ibans found in IbanData.hs (imported qualified as Data). 
    Ran with ghci and displaying values invalid~ and validTestReport
--}

module IbanValidation where
    import Lib
    import Data.List
    import qualified IbanData as Data

    sanitise :: [Char] -> [Char]
    sanitise [] = []
    sanitise (x:xs) = if x == ' ' then sanitise xs else x : (sanitise xs) 

    countryLengthTable c = lookup c $ [("AL", 28), ("AD", 24), ("AT", 20), ("AZ", 28), ("BH", 22), ("BY", 28), ("BE", 16), ("BA", 20), ("BR", 29), ("BG", 22), ("CR", 22), ("HR", 21), ("CY", 28), ("CZ", 24), ("DK", 18), ("DO", 28), ("SV", 28), ("EE", 20), ("FO", 18), ("FI", 18), ("FR", 27), ("GE", 22), ("DE", 22), ("GI", 23), ("GR", 27), ("GL", 18), ("GT", 28), ("VA", 22), ("HU", 28), ("IS", 26), ("IQ", 23), ("IE", 22), ("IL", 23), ("IT", 27), ("JO", 30), ("KZ", 20), ("XK", 20), ("KW", 30), ("LV", 21), ("LB", 28), ("LI", 21), ("LT", 20), ("LU", 20), ("MT", 31), ("MR", 27), ("MU", 30), ("MD", 24), ("MC", 27), ("ME", 22), ("NL", 18), ("MK", 19), ("NO", 15), ("PK", 24), ("PS", 29), ("PL", 28), ("PT", 25), ("QA", 29), ("RO", 24), ("LC", 32), ("SM", 27), ("ST", 25), ("SA", 24), ("RS", 22), ("SC", 31), ("SK", 24), ("SI", 19), ("ES", 24), ("SE", 24), ("CH", 21), ("TL", 23), ("TN", 24), ("TR", 26), ("UA", 29), ("AE", 23), ("GB", 22), ("VG", 24)]
    
    pass1 :: [Char] -> Bool
    pass1 input = 
                let inputLength = length input
                    expectedLength = countryLengthTable (take 2 input)
                in case expectedLength of   Nothing -> False
                                            Just x -> inputLength == x
                                        
    pass2 :: [Char] -> [Char]
    pass2 input = drop 4 input ++ take 4 input 

    codeTable c = lookup c $ zip ['A' .. 'Z'] [10 .. 35]

    pass3 :: [Char] -> [Char]
    pass3 input = concat $ map (\x -> predicate x) input
                where
                    predicate x = case (codeTable x) of Nothing -> [x]
                                                        Just x -> show x

    pass4 :: [Char] -> Integer
    pass4 input = (read input :: Integer) `mod` 97

    isValid :: [Char] -> Bool
    isValid input = if pass1 input' then ibanTest input' else False
                    where 
                        input' = sanitise input
                        ibanTest = ((== (1) ) . pass4 . pass3 . pass2)
    
    validTestResult = forall Data.knownValid (\x -> isValid x)
    invalidTestResult = forall Data.knownInvalid (\x -> (not . isValid) x)