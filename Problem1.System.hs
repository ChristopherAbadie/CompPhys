import CodeWorld
import Data.Text (pack)
import Text.Printf (printf)
import System.Random(getStdGen,random)

main = do
  gen <- getStdGen
  simulationOf (initial gen) update view

initial gen = ((0 :: Double), (gen,[],0 :: Double,0 :: Double,0 :: Double,0 :: Double))

updateStep (gen,x',y',counter,avg,mo2,mo3) = (gen',x,y, counter',avg',mo2',mo3')
    where
    (x,y,gen') = next(gen)
    counter'= counter+1.0
    avg'= (avg*2*counter+x+y)/(2*counter')
    mo2' = (mo2*2*counter+(x)**2+(y)**2)/(2*counter')
    mo3' = (mo3*2*counter+(x)**3+(y)**3)/(2*counter')

loop 0 f x = x
loop n f x = loop (n-1) f (f(x))

update dt (time,(gen,pts,counter,avg,mo2,mo3)) = (time+dt,(gen',pts',counter',avg',mo2',mo3'))
  where
  pts'=(x,y):pts
  (gen',x,y,counter',avg',mo2',mo3') = loop 1000 updateStep (gen,0,0,counter,avg,mo2,mo3)

view (time,(gen,pts,counter,avg,mo2,mo3)) =
  translated 0 9.5 (text (pack message1))
  & translated 0 8.5 (text (pack message2))
  & translated 0 7.5 (text (pack message3))
  & pictures[translated (20*x-10) (17*y-10) dot | (x,y) <- pts]
  where
  dot = colored blue (solidCircle(0.1))
  message1 = printf "Points: %.0f  " counter
  message2 = printf "Average: %.3f 2nd Moment: %.3f" avg mo2
  message3 = printf "3rd Moment: %.3f Time: %.2f" mo3 time
 
next(gen) =
  let (x,gx) = random(gen)
      (y,gy) = random(gx)
  in
  ((x :: Double),(y :: Double),gy)
