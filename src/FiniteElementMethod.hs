module FiniteElementMethod
    ( finiteElementMethod
    ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

listOfElements :: Int -> [Double -> Double]
listOfElements n = [makeFirst nNum] ++ (makeElements nNum 2 []) ++ [makeLast nNum]
                where nNum = fromIntegral n
                      makeFirst n = \x -> case () of
                                         _  | x <= 2/n  -> 1 - n/2 * x
                                            | otherwise -> 0 
                      makeElement n k = \x -> case () of
                                         _  | x >= 2/n * (k-2) && x <= 2/n * (k-1) -> 2-k + n/2 * x
                                            | x > 2/n * (k-1) && x <= 2/n * k      -> k - n/2 * x
                                            | otherwise                            -> 0
                      makeLast n = \x -> case () of
                                         _  | x >= 2/n * (n-1) -> 1-n + n/2 * x
                                            | otherwise        -> 0
                      makeElements n k xs   | k <= n    = xs ++ [makeElement n k] ++ makeElements n (k+1) xs
                                            | otherwise = xs

derivative :: (Double -> Double) -> (Double -> Double)
derivative f = \x -> (f (x+h) - f (x-h)) / (2*h)
         where h = 0.0001

functionsProduct :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
functionsProduct f g = \x -> f x * g x 

-- | exact polynomials integration using Gaussian quadrature
integralOfPolynomial :: Int -> (Double -> Double) -> Double
integralOfPolynomial n f = sum [(1 / nNum) * (f ( (1 / nNum) * ((1 / sqrt 3) + 2*k + 1) ) 
                                           +  f ( (1 / nNum) * (- (1 / sqrt 3) + 2*k + 1) )) | k <- [0..(nNum-1)]]
                where nNum = fromIntegral n

bilinearForm :: Int -> (Double -> Double) -> (Double -> Double) -> Double
bilinearForm n u v = - (u 2) * (v 2) 
                   + (integralOfPolynomial n $ functionsProduct (derivative u) (derivative v))
                   - (integralOfPolynomial n $ functionsProduct u v) 

linearForm :: (Double -> Double) -> Double
linearForm v = (v 2) * 2

{-| calculating values in the system of equations
    | b1 c1 0  0  ... 0 |  | v1 |   | d1 |
    | a2 b2 c2 0  ... 0 |  | v2 |   | d2 |
    | 0  a3 b3 c3 ... 0 |  | v3 |   | d3 |
    | .  .  .  .  .   . |  | .  | = | .  |
    | .  .  .  .   .  . |  | .  |   | .  |
    | .  .  .  .    . . |  | .  |   | .  |
    | 0  ...  0 0 an bn |  | vn |   | dn |
-}

allValues :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
allValues (a:as) (b:bs) (c:cs) (d:ds) = [(d - c * ndValue) / b] ++ (ndValue:values)
                                 where (ndValue:values) = ithValue (a:as) (b:bs) (c:cs) (d:ds)

ithValue :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
ithValue (a:as) (b:bs:bss) (c:cs) (d:ds:dss) = do
   let weight = a / b
   let bs' = bs - weight * c
   let ds' = ds - weight * d

   let xs = ithValue as (bs':bss) cs (ds':dss)
   if cs == [] then return (ds' / bs') ++ xs
               else return ( (ds' - (head cs) * (head xs)) / bs' ) ++ xs

-- | factors in the system of equations
aValues :: [Double -> Double] -> Int -> [Double]
aValues (e:es:[]) n = [bilinearForm n es e]
aValues (e:es:ess) n = [bilinearForm n es e] ++ aValues (es:ess) n

bValues :: [Double -> Double] -> Int -> [Double]
bValues (e:[]) n = [bilinearForm n e e]
bValues (e:es) n = [bilinearForm n e e] ++ bValues es n

cValues :: [Double -> Double] -> Int -> [Double]
cValues (e:es:[]) n = [bilinearForm n e es]
cValues (e:es:ess) n = [bilinearForm n e es] ++ cValues (es:ess) n

dValues :: [Double -> Double] -> [Double]
dValues (e:[]) = [linearForm e]
dValues (e:es) = [linearForm e] ++ dValues es

-- | Thomas Algorithm to solve system of equations represented by tridiagonal matrix
thomasAlgorithm :: Int -> [Double -> Double] -> [Double]
thomasAlgorithm n es = allValues (0:(aValues (tail es) n)) (1:(bValues (tail es) n)) (0:(cValues (tail es) n)) (0:(dValues (tail es)))

-- | sum of all the elements of the list is the approximation of the given differential equation
sumAsList :: Int -> [Double -> Double]
sumAsList n = (\(e, v) -> (*v) . e) <$> (zip es values)
               where es = listOfElements n
                     values = thomasAlgorithm n es

-- | calculating points
makePoints :: [Double] -> [Double -> Double] -> [(Double, Double)]
makePoints xs vs = [(x, calculate x vs) | x <- xs]
               where calculate x vs = sum [v x | v <- vs]

-- | drawing chart
finiteElementMethod :: Int -> IO ()
finiteElementMethod n = toFile def "chart.svg" $ do
   layout_title .= "Approximation of Acoustic Wave Equation"
   setColors [opaque blue, opaque red]
   let listOfValues = sumAsList n
   plot (line "line" [makePoints [0,(0.05)..2] listOfValues])
   plot (points "points" (makePoints [0,(0.05)..2] listOfValues))