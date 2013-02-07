divble2 :: Integral a => a -> Bool
divble2 n = all (\m -> n `mod` m == 0) [1..20]