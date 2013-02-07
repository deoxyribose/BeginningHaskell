import Data.List
import Data.Char (digitToInt)

bouncy :: (Integral a) => a -> Bool
bouncy x
	| sort (listify x) == (listify x) = False
	| reverse (sort (listify x)) == (listify x) = False
	| otherwise = True
	where listify x = map digitToInt (show x)

proportion x = fromIntegral antalbouncy/fromIntegral x
	where antalbouncy = length $ filter bouncy [1..x]

--zip [1..] (map bouncy [1..])


prop2 :: (Fractional a) => (a,a) -> [Bool] -> [a]
prop2 _ [] = []
prop2 (num, denom) (True:xs) = (num +1) / (denom +1) : prop2 (num +1, denom +1) xs
prop2 (num, denom) (False:xs) = num / (denom +1) : prop2 (num, denom +1) xs

--findleast :: [Int] -> [Int]
--findleast y
--	| (length y) / (last y) == 0.99 = y
--	| bouncy inc = findleast (inc:y)
--		where inc = succ (last y)

main = putStrLn . head . dropWhile ((<0.99) . snd) . zip [1..] (prop2 (0,0) $ map bouncy [1..])