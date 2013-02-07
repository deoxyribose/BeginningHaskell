import Data.List (sort)
import Data.Char (digitToInt)

samedigitsaspoly :: (Integral a) => a -> a -> Bool
samedigitsaspoly x p
	| sort (map digitToInt (show x)) == sort (map digitToInt (show (p*x))) = True
	| otherwise = False

permmulti :: (Integral a) => [a] -> a
permmulti y = head $ filter (\x -> all (samedigitsaspoly x) [2..6]) y