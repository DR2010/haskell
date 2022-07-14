{-|
Module      : DanDouble
Author      : Debashish Chakraborty, Your name and UID here
Date        : 04/02/2019
Description : This module contains functions to calculate GPA from grades and marks.
-}

main = do   
    let (a, b) = qEquation (1,-20,9)            
    putStrLn $ "a= " ++ show a ++ " b= " ++ show b

    let x = markToGrade( 77 )  
    putStrLn $ "grade = " ++ show x

    let x = markToGrade( 77 )  
    putStrLn $ "grade = " ++ show x

    let x = gradeToGPA( HighDistinction )
    putStrLn $ "gradeToGPA = " ++ show x

    let x = maybeGradeToGPA( Just HighDistinction )
    putStrLn $ "maybeGradeToGPA = " ++ show x

    let x = gradeToGPA ( markToGrade( 77 ) )
    putStrLn $ "gradeToGPA = " ++ show x

    let x = markToGPA ( Just 77 )
    putStrLn $ "markToGPA = " ++ show x

    let f = fib 20
    putStrLn $ "fib = " ++ show f

    let lw = leftWall 20
    putStrLn $ "leftWall = " ++ show lw

    let check = checkHit 20 10
    putStrLn $ "checkHit = " ++ show check

    let check = checkHitPoint (20, 10) (20, 10)
    putStrLn $ "checkHitPoint = " ++ show check


    let check = checkHitPoint (20, 10) (21, 11)
    putStrLn $ "checkHitPoint = " ++ show check
    
    let check = checkHitPointArray (20, 10) [(21, 11), (22, 12), (23, 13), (20, 10) ]
    putStrLn $ "checkHitPointArray P1= " ++ show check

    let check = checkHitPointArray (20, 10) [(21, 11), (22, 12), (23, 13)  ]
    putStrLn $ "checkHitPointArray P2= " ++ show check


    let check = listIncrease 1 10 
    putStrLn $ "listIncrease = " ++ show check

    let check = listIncreasex 1 10 
    putStrLn $ "listIncreasex = " ++ show check

    let check = listIncreasey 1 10
    putStrLn $ "listIncreasey = " ++ show check


data Grade = Fail | Pass | Credit | Distinction | HighDistinction
   deriving Show

type Course = String
-- type GP = Double
type GPA = Double
-- type Mark = Double
type Mark = Int

type Point = (Integer, Integer)


listIncrease :: Int -> Int -> [[Int]]
listIncrease a b = [[a..i] | i <- [a..a+b-1]]

listIncreasex :: Int -> Int -> [[Int]]
listIncreasex a b = [[a..i] | i <- [b]]

listIncreasey :: Int -> Int -> [Int]
listIncreasey a b = [a..b]

leftWall ::  Integer -> Integer
leftWall 0 = 0
leftWall n = leftWall (n-1) 

checkHit ::  Integer -> Integer -> Bool
checkHit mark hit
  | mark == hit = True
  | otherwise   = False

checkHitPoint ::  Point -> Point -> Bool
checkHitPoint mark hit
  | mark == hit = True
  | otherwise   = False

checkHitPointArray :: Eq a => a -> [a] -> Bool
checkHitPointArray a [] = False
checkHitPointArray a (x:xs)
  | a == x = True
  | otherwise = isElement a xs

isElement :: Eq a => a -> [a] -> Bool
isElement a [] = False
isElement a (x:xs)
  | a == x = True
  | otherwise = isElement a xs


fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



-- | Exercise 7
-- Your comment here
markToGPA :: Maybe Double -> Double
markToGPA mark = case mark of
  Just d -> gradeToGPA ( markToGrade( d ) )
  Nothing -> 0

    -- markToGPA -> 75 -> GPA #
    --  input a mark
    --  with mark you pass to markToGrade
    --  with grade, we pass to gradeToGPA 


-- | Exercise 3
-- Convert marks to grade
markToGrade ::  Double -> Grade
markToGrade mark
  | mark >= 80 && mark <= 100 = HighDistinction
  | mark >= 70 && mark <   80 = Distinction
  | mark >= 60 && mark <   70 = Credit
  | mark >= 50 && mark <   60 = Pass
  | mark >=  0 && mark <   50 = Fail
  | otherwise = error "Not a valid mark"  


-- | Exercise 4
-- Your comment here
markToGrade' :: (String, Double) -> Grade
markToGrade' (_, mark) = markToGrade mark



-- | Exercise 6
-- Your comment here
gradeToGPA ::  Grade -> Double
gradeToGPA grade = case grade of
  HighDistinction -> 7
  Distinction -> 6
  Credit -> 5
  Pass -> 4
  Fail -> 0

maybeGradeToGPA :: Maybe Grade -> Double
maybeGradeToGPA grade = case grade of
  Just HighDistinction -> 7
  Just Distinction -> 6
  Just Credit -> 5
  Just Pass -> 4
  Just Fail -> 0
  Nothing -> 0


qEquation :: (Float, Float, Float) -> (Float, Float)
qEquation (a, b, c) = (f1, f2)
    where
           x1 = e + sqrt d / (2 * a)
           x2 = e - sqrt d / (2 * a)
           d = b * b - 4 * a * c
           e = - b / (2 * a)
           f1 = x1 * (-1)
           f2 = x2 * (-1)


markToGradeSafe ::  Double -> Maybe Grade
markToGradeSafe mark 
  | mark >= 0 && mark <= 100 = Just (markToGrade mark)
  | otherwise = Nothing

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action


loop :: Int -> (IO()) -> IO()
loop 0 _ = return ()
loop n f =
 do
  f
  loop (n - 1) f
