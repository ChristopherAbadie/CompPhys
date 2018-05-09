{-# LANGUAGE ParallelListComp #-}
main =
  let
    radius=0.8
    q=0.5/(10^(9))
    k=9*10^9
    vin=k*q/radius
    vout(r)=k*q/r

    vinList1 = [k*q/radius | r<-[0.001, 0.201..0.601]]
    voutList1 = [k*q/r | r<-[0.801,1.001..4.001]]
    vList1 = concat[vinList1, voutList1]

    vinList2 = [k*q/radius | r<-[0.021, 0.221..0.621]]
    voutList2 = [k*q/r | r<-[0.821,1.021..4.021]]
    vList2 = concat[vinList2, voutList2]
    eField = [(v1-v2)/0.02 | v2 <- vList2 | v1 <- vList1]

   in do
     writeFile "Problem1.csv" 
       (concat[show rad ++ ", " ++ show vol ++ ", " ++ show ef ++ "\n"  
         | rad <- [0.001,0.201..4.001]
         | vol <- vList1 
         | ef <- eField])

   

