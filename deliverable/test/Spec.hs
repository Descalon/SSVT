import Test.QuickCheck
import Triangles
import Derangement
import ROT13

triangleTests = do
        quickCheck prop_rect
        quickCheck prop_rect2
        quickCheck prop_rect3
        quickCheck prop_isoceles
        quickCheck prop_isoceles2
        quickCheck prop_isoceles3
        quickCheck prop_equilateral
        quickCheck prop_noTriangle

derangementTests = do
        quickCheck test_Should_Disorder

rot13Tests = do
        quickCheck prop_same_length
        quickCheck prop_reversable

main :: IO ()
main = do 
        putStrLn "-----"
        putStrLn "triangles"
        _ <- triangleTests
        putStrLn "-----"
        putStrLn "derangement"
        _ <- derangementTests
        putStrLn "-----"
        putStrLn "ROT13"
        _ <- rot13Tests
        putStrLn "-----"
        
        return ()
