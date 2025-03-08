{-# LANGUAGE DeriveGeneric #-}

module Main where

main :: IO ()
main = print $ 


readNumbers :: String -> String
readNumbers xs = show . sum . map read $ words xs

round5 :: Int -> Int
round5 x 
    | x < 38 = x
    | (m5 - x) < 3 = m5
    where m5 = x + (5 - x `mod` 5)

solveAO :: [Int] -> [Int]
solveAO (s:t:a:b:m:_:rest) = [as, os]
    where as = length $ filter (\x -> s <= x && x <= t) $ map (\x -> x + a) $ take m rest
        os = length $ filter (\x -> s <= x && x <= t) $ map (\x -> x + b) $ drop m rest 