{-# LANGUAGE ParallelListComp #-}
import GeneratePW (gauss)

initial :: Double
initial = 0

final :: Double
final = l

l=0.5
k = 9*10^9
lam = 2*1/(10^10)
d = 0.1
f(x) = k*lam*1/((x^2 + d^2)**0.5) 

exact = k*lam*log((l+(l^2+d^2)**0.5)/d)

simpson fn init final npoints = partresult*dt
    where
        dt = (final - init) / fromIntegral(npoints-1)
        times = [(init + dt*fromIntegral(i)) | i <- [1..npoints-2]]
        weights = cycle [4/3,2/3]
        partresult = sum [(fn t) *w | t <- times | w <- weights] + (fn init + fn final)/3.0 

main = 
  let 
    npoints = 513
    
    i_simpson = simpson f initial final
    simpsonValue = i_simpson npoints
    relerror = abs((simpsonValue - exact)/exact)    

  in do
    writeFile "Problem4.csv" 
      (concat[show npoints ++ ", " ++ show simpsonValue  
      ++ ", " ++ show exact ++ ", " ++ show relerror ++ "\n"])
