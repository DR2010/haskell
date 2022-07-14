{-

Module  :  Area
Author  :  COMP1100/COMP1130 Team, Kevin

TODO: Write some comment here about the module.

-}

module Area where

main = do
    putStrLn $ "test"

addOne :: Double -> Double
addOne int = int + 1

areaSquare :: Double -> Double
areaSquare len = len^2

rect :: Double -> Double -> Double
rect w h = w * h

areaTriangle :: Double -> Double -> Double -> Double
areaTriangle a b c = (s*(s-a)*(s-b)*(s-c))**(0.5)
    where
        s :: Double 
        s = (a+b+c)/2

isValidTriangle :: Double -> Double -> Double -> Bool
isValidTriangle a b c 
    | a + b > c && a + c > b && b + c > a = True
    | otherwise = False

areaOfTriangleSafe :: Double -> Double -> Double -> Double
areaOfTriangleSafe a b c 
    | isValidTriangle a b c = areaTriangle a b c
    | otherwise = 0

areaOfTriangleSafeM :: Double -> Double -> Double -> Maybe Double
areaOfTriangleSafeM a b c 
    | isValidTriangle a b c = Just ( areaTriangle a b c )
    | otherwise = Nothing



