import Test.QuickCheck

import FiniteElementMethod

prop_productOfDistantElements :: Int -> Double -> Bool
prop_productOfDistantElements n x = checkEqual0 (elements n) x 
                                    where elements n = listOfElements n
                                          checkEqual0 (f:g:h:fs) x = f x * h x == 0 && checkEqual0 (g:h:fs) x
                                          checkEqual0 _ _ = True

main :: IO ()
main = do
    putStrLn "\n*** Testing prop_productOfDistantElements... ***"
    quickCheck (withMaxSuccess 1000 prop_productOfDistantElements)
