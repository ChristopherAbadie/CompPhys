{-# LANGUAGE ParallelListComp #-}

main=
  let
    h=0.1
    f(x) = (3-30*x^2+35*x^4)/8
    f'(x) = (-60*x+140*x^3)/8

    f'fd(x) = (f(x+h)-f(x))/h
    fdErList = [abs(f'fd(x)-f'(x)) | x <- [-1,-0.9..1]]

    f'cd(x) = (f(x+h/2)-f(x-h/2))/h
    cdErList = [abs(f'cd(x)-f'(x)) | x <- [-1,-0.9..1]]

    f'ed(x) = 1/(3*h) * (8*(f(x+h/4)-f(x-h/4))-(f(x+h/2)-f(x-h/2)))
    edErList = [abs(f'ed(x)-f'(x)) | x <- [-1,-0.9..1]]

  in do
    writeFile "Problem4.csv" 
      (concat[show x ++ ", " ++ show (f(x)) ++ ", " ++ show fd ++ ", " ++ show cd 
      ++ ", " ++ show ed ++ "\n"
        | x <- [-1,-0.9..1]
        | fd <- fdErList 
        | cd <- cdErList
        | ed <- edErList ])
