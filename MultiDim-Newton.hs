{-# LANGUAGE ParallelListComp #-}

import GaussElim

default (Int, Double)
type Vector = [Double]

-- Multidemensional Newton-Raphson
root = newton jacobFunction y0
     where
        y0 = [0.1,19.6]

-- Newton-Raphson root finding from Lab 4. 
newton fdf x =
    (getroot . head . dropWhile not_converged . take 15 . iterate step . setdata bigNumber) x
    where
    getroot(_,x,_) = x
    setdata x_ x = (x_,x,fdf x)
    step (_,x,(fx,jx)) = setdata x (x -: linearSolve(jx,fx))
    not_converged (x_,x,(fx, jx)) = magnitude (fx) > eps
    eps = 1e-8
    bigNumber = constVec (1/0) x

-- Magnitude of vector x; |x|=sqrt[(x_1)^2+ (x_2)^2+...]
magnitude :: Vector -> Double 
magnitude (x)= sqrt(sum [x1*x1 |x1 <- x])

-- Multiplication of a scalar and a vector
(*:) :: Double -> Vector -> Vector
s *: v = [s * x | x <- v]
infixl 7 *:

-- Vector subtraction
(-:) :: Vector -> Vector -> Vector
v1 -: v2 = [x1 - x2 | x1 <- v1 | x2 <- v2]
infixl 6 -:


jacobFunction :: Vector -> (Vector,[[Double]])
jacobFunction y =  
   let 
     m = 2
     mu = 0.4
     g = 9.81
     f = [ m * g * cos(y !! 0) - (y !! 1)
         , m * g * sin(y !! 0) - mu * (y !! 1)]
     j =
       [ [ (-1) * m * g * sin(y !! 0), (-1)]
       , [ m * g * cos(y !! 0), (-1) * mu]
       ]
    in
        (f,j)

