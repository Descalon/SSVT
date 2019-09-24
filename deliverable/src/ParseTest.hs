{--Exercise 2 (10 min, more like 90 if counting the generator for exercise 4)
    Using the generator we made for exercise 4, we can test that an arbitrary Form will parse to itself (by using show)
--}

module ParseTest where
    import Lib
    import Generator

    prop_should_be_same :: Form -> Bool
    prop_should_be_same f = post ((head . parse) f')
                        where 
                            f' = show f
                            post x = (show x) == f'
    