crossproduct :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
crossproduct (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-b3*a1, a1*b2-b1*a2)