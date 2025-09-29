{-# OPTIONS_GHC -Wall #-}
module Yermolovych04 where

-- Код - просто список символів - десяткових цифр '0' ..'9'
type Code = String

-- Крок гри (Move)
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches (x:xs) (y:ys)
  | x == y    = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys
exactMatches _ _ = 0

-- Задача 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits []     = replicate 10 0
countDigits (c:cs) =
  let rest = countDigits cs
      d    = fromEnum c - fromEnum '0'
  in take d rest ++ [rest !! d + 1] ++ drop (d+1) rest

-- Задача 3 -----------------------------------------
matches :: Code -> Code -> Int
matches xs ys = sum (zipWith min (countDigits xs) (countDigits ys))

-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att =
  let bulls = exactMatches cd att
      cows  = matches cd att - bulls
  in Move att bulls cows

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move att b c) cd = getMove cd att == Move att b c

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes mv (cd:cds)
  | isConsistent mv cd = cd : filterCodes mv cds
  | otherwise          = filterCodes mv cds

-- Задача 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes 0 = [""]
allCodes n = [ d : code | d <- ['0'..'9'], code <- allCodes (n-1) ]

-- Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = go (allCodes (length cd))
  where
    go [] = []
    go (att:rest) =
      let mv = getMove cd att
      in mv : if att == cd
              then []
              else go (filterCodes mv rest)