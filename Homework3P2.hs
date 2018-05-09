{-# LANGUAGE ParallelListComp #-}
import GeneratePW (gauss)

initial :: Double
initial = -1.0

final :: Double
final = 1.0

f(-1) = -24
f(-0.75) = -16.9063
f(-0.50) = -11.5
f(-0.25) = -7.5938
f(0) = -5
f(0.25) = -3.5313
f(0.50) = -3
f(0.75) = -3.2188
f(1) = -4 

trapezoid fn init final npoints = partresult*dt
    where
        dt = (final - init) / fromIntegral(npoints-1)
        times = [(init + dt*fromIntegral(i)) | i <- [1..npoints-2]]
        partresult = sum [fn t  | t <- times] + 0.5*(fn init + fn final) 

simpson fn init final npoints = partresult*dt
    where
        dt = (final - init) / fromIntegral(npoints-1)
        times = [(init + dt*fromIntegral(i)) | i <- [1..npoints-2]]
        weights = cycle [4/3,2/3]
        partresult = sum [(fn t) *w | t <- times | w <- weights] + (fn init + fn final)/3.0 

gaussintegral fn init final npoints = partresult 
    where
        partresult = sum [(fn t) * w | (t,w) <- gauss npoints ] 

main = 
  let 
    npoints = 9
    
    i_trapezoid = trapezoid f initial final
    trapezoidValue = i_trapezoid npoints

    i_simpson = simpson f initial final
    simpsonValue = i_simpson npoints
  
--    i_gauss = gaussintegral f initial final
--    gaussValue = i_gauss npoints
  
  in do
    writeFile "Problem2.csv" 
      (concat[show npoints ++ ", " ++ show trapezoidValue
      ++ ", " ++ show simpsonValue  ++ "\n"])
