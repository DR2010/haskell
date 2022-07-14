module Area where

areaSquare :: Double -> Double
areaSquare a = a*a

qEquation :: (Float, Float, Float) -> (Float, Float)
qEquation (a, b, c) = (x1, x2)
    where
           x1 = e + sqrt d / (2 * a)
           x2 = e - sqrt d / (2 * a)
           d = b * b - 4 * a * c
           e = - b / (2 * a)

qFactor :: (Float, Float, Float) -> (Float, Float)
qFactor (a, b, c) = (f1, f2)
    where
           x1 = e + sqrt d / (2 * a)
           x2 = e - sqrt d / (2 * a)
           d = b * b - 4 * a * c
           e = - b / (2 * a)
           f1 = x1 * (-1)
           f2 = x2 * (-1)