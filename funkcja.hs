module Main where

jakasFunkcja :: String -> Bool 
jakasFunkcja x = x == "2"

main = do
	x <- jakasFunkcja "23";
	putStrLn "halo";
