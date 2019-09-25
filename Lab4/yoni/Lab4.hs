import SetOrd
import System.Random
import Test.QuickCheck

getRandInt :: Int -> IO (Set Int)
getRandInt n = do 
    g <- newStdGen
    return( Set (take n (randomRs(1, 9) g)))

