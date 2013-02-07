import Data.List (elemIndex, group)

longestcycle :: (Integral a) => [a] -> a
longestcycle xs = maybe (error "maximum must exist") (xs!!) $ elemIndex (maximum cyclelengths) cyclelengths where
	cyclelengths = cyclelengths' xs

cyclelengths' :: (Integral a) => [a] -> [Int]
cyclelengths' = map unitfraction

unitfraction :: (Integral a) => a -> Int
unitfraction d = length . concat . concat $ recurring (group $ take (1050) $ infiniDiv 1 d) []

--longestcycle [2..10] = 7

infiniDiv :: (Integral a) => a -> a -> [String]
infiniDiv _ 0 = error "Division by zero makes me angry!"
infiniDiv 0 y = []
infiniDiv x y = (z'):infiniDiv (rem x' y) y where
	x' = if x > y then x else 
		x * 10^ylen
	ylen = fromIntegral (length (show y) - length (show x) + 1)
	z    = show $ div x' y
	z'   = (take (ylen - length z) $ repeat '0') ++ z

recurring :: (Eq a) => [a] -> [a] -> [a]
recurring [] _ = []

recurring (x:xs) ys | null recurr = recurring xs (ys ++ [x])
                  | otherwise   = recurr
	where recurr = dropWhile (/=x) ys
