main = do   
    let (a, b) = qEquation (1,-20,9)            
    putStrLn $ "a= " ++ show a ++ " b= " ++ show b


  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  

qEquation :: (Float, Float, Float) -> (Float, Float)
qEquation (a, b, c) = (f1, f2)
    where
           x1 = e + sqrt d / (2 * a)
           x2 = e - sqrt d / (2 * a)
           d = b * b - 4 * a * c
           e = - b / (2 * a)
           f1 = x1 * (-1)
           f2 = x2 * (-1)




