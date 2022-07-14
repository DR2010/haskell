{-|
Module      : GPACalculator
Author      : Debashish Chakraborty, Your name and UID here
Date        : 04/02/2019
Description : This module contains functions to calculate GPA from grades and marks.
-}

main = do   
    let (a, b) = qEquation (1,-20,9)            
    putStrLn $ "a= " ++ show a ++ " b= " ++ show b

    let x = markToGrade( 77 )  
    putStrLn $ "grade = " ++ show x

    let x = markToGrade( 98 )  
    putStrLn $ "grade = " ++ show x

    let x = gradeToGPA( HighDistinction )
    putStrLn $ "gradeToGPA = " ++ show x

    let x = maybeGradeToGPA( Just HighDistinction )
    putStrLn $ "maybeGradeToGPA = " ++ show x

    -- let result1 = maybe False odd (Just value1)

    -- markToGPA -> 75 -> GPA #
    --   input a mark
    --   with mark you pass to markToGrade
    --  with grade, we pass to gradeToGPA 

data Grade = Fail | Pass | Credit | Distinction | HighDistinction
   deriving Show

type Course = String
type GP = Double
type GPA = Double
type Mark = Double
-- type Mark = Int


-- | Exercise 7
-- Your comment here
-- markToGPA :: Mark -> GPA
-- markToGPA mark 
--     x = markToGrade( mark ).

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
markToGrade' :: (Course, Mark) -> Grade
markToGrade' (_, mark) = markToGrade mark


-- | Exercise 5
-- Your comment here
markToGradeSafe ::  Mark -> Maybe Grade
markToGradeSafe mark
  | mark >= 0 && mark <= 100 = Just (markToGrade mark)
  | otherwise = Nothing

-- | Exercise 6
-- Your comment here
gradeToGPA :: Grade -> GPA
gradeToGPA grade = case grade of
  HighDistinction -> 7
  Distinction -> 6
  Credit -> 5
  Pass -> 4
  Fail -> 0

maybeGradeToGPA :: Maybe Grade -> GPA
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



