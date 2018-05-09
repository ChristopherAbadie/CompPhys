{-# LANGUAGE ParallelListComp #-}

main=
  let
    tau = 2
    y_exact t = y0*(exp(-t/tau))
    logistic(t, y) = -y/tau
    t0 = 0.0
    y0 = 5

    solutions1forw = takeWhile(withinDomain(0.0,4.0))(iterate(step euler logistic 0.1)(t0,y0))
    alldata1forw = [(t, logistic(t,y), y, y_exact t, abs(y - y_exact t)) | (t, y) <- solutions1forw]
    output1forw = [(show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e ++ "\n")
      | (a,b,c,d,e) <- alldata1forw] 
    output1 = concat[output1forw] 

  in do
    writeFile "Problem3.csv" (concat(output1))

euler f h t y = y + k1
    where
      k1 = h * f(t,y)
-----------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------

step method f h (t,y) = (t + h, method f h t y)

withinDomain (a,b) (t,y) = a <= t && t <= b

