import CodeWorld
import Data.Text (pack)
import Text.Printf (printf)
import System.Random(getStdGen,random)

main = do
  gen <- getStdGen
  simulationOf (initial gen) update view

initial gen = ((0 :: Double), (gen,[],0 :: Double,0 :: Double,0 :: Double))

updateStep (gen,x',y',counter,npond,pi) = (gen',x,y, counter',npond',pi')
    where
    (x,y,gen') = next(gen)
    counter'= counter+1.0
    npond'= 
      if y<sqrt(1-x^2) then npond+1.0
      else npond
    pi' = (4*npond/counter)

loop 0 f x = x
loop n f x = loop (n-1) f (f(x))

update dt (time,(gen,pts,counter,npond,pi)) = (time+dt,(gen',pts',counter',npond',pi'))
  where
  pts'=(x,y):pts
  (gen',x,y,counter',npond',pi') = loop 1000 updateStep (gen,0,0,counter,npond,pi)

view (time,(gen,pts,counter,npond,pi)) =
  translated 0 9.5 (text (pack message1))
  & translated 0 8.5 (text (pack message2))
  & translated 0 7.5 (text (pack message3))
  & pictures[translated (20*x-10) (17*y-10) dot | (x,y) <- pts]
  where
  dot = colored blue (solidCircle(0.1))
  message1 = printf "Points: %.0f  " counter
  message2 = printf "NPond: %.0f Pi: %.8f" npond pi
  message3 = printf "Time: %.2f" time
 
next(gen) =
  let (x,gx) = random(gen)
      (y,gy) = random(gx)
  in
  ((x :: Double),(y :: Double),gy)
