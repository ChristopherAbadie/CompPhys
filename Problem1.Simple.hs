import CodeWorld
import Data.Text (pack)
import Text.Printf (printf)

seed=31 :: Int
maxrange = 112233 :: Int
a=9999 :: Int
c=11 :: Int

main = simulationOf initial update view

initial = ((0 :: Double), (seed,[],0 :: Double,0 :: Double,0 :: Double,0 :: Double))

updateStep (random,x',y',counter,avg,mo2,mo3) = (random',x,y, counter',avg',mo2',mo3')
    where
    (x,y,random') = next(random)
    counter'= counter+1.0
    avg'= (avg*2*counter+x+y)/(2*counter')
    mo2' = (mo2*2*counter+(x)**2+(y)**2)/(2*counter')
    mo3' = (mo3*2*counter+(x)**3+(y)**3)/(2*counter')

loop 0 f x = x
loop n f x = loop (n-1) f (f(x))

update dt (time,(random,pts,counter,avg,mo2,mo3)) = (time+dt,(random',pts',counter',avg',mo2',mo3'))
  where
  pts'=(x,y):pts
  (random',x,y,counter',avg',mo2',mo3') = loop 1000 updateStep (random,0,0,counter,avg,mo2,mo3)

view (time,(random,pts,counter,avg,mo2,mo3)) =
  translated 0 9.5 (text (pack message1))
  & translated 0 8.5 (text (pack message2))
  & translated 0 7.5 (text (pack message3))
  & pictures[translated (20*x-10) (17*y-10) dot | (x,y) <- pts]
  where
  dot = colored blue (solidCircle(0.1))
  message1 = printf "Points: %.0f  " counter
  message2 = printf "Average: %.3f 2nd Moment: %.3f" avg mo2
  message3 = printf "3rd Moment: %.3f Time: %.2f" mo3 time
 
nextRandom(n) = mod (a*n+c) (maxrange)

next(random) =
Ê let x = random
Ê Ê Ê y = nextRandom(x)
Ê Ê Ê r = nextRandom(y)
Ê in
Ê (normal(x),normal(y),r)
  
normal(x) = fromIntegral(x) / maxrange'

maxrange' :: Double
maxrange' = fromIntegral(maxrange)

