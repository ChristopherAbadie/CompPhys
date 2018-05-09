{-# LANGUAGE ParallelListComp #-}

import GaussElim

main =
  let
    b[1]=3
    b[2]=2.03
    b[3]=1.16
    b[4]=0.44
    b[5]=0.02
    
    a = [([2,3,4,5],b[1]),([2,3,4],b[2]),([2,3],b[3]),([2],b[4]),([],b[5])]    

    xList = backSubst a

  in do
    writeFile "Problem1.csv" 
      (concat[show ex ++ ", " ++ show exl  ++ "\n"  
        | ex <- [1..5]
        | exl <- xList])


