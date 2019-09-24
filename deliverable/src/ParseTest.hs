module ParseTest where
    import Lib
    import Generator

    prop_should_be_same :: Form -> Bool
    prop_should_be_same f = post ((head . parse) f')
                        where 
                            f' = show f
                            post x = (show x) == f'
    