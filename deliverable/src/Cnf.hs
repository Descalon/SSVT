module Cnf where
    import Lib

    toNnf :: Form -> Form
    toNnf = arrowFree . toNnf

    distribution Csj fs = 