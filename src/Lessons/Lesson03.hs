{-# LANGUAGE InstanceSigs #-}
-- | Module is a unit of compilation.

module Lessons.Lesson03 (sumOfInts, sumOfInts') where

sumOfInts :: [Int] -> Int
sumOfInts [] = 0
sumOfInts (h:t) = h + sumOfInts t --no recursion

sumOfInts' :: [Int] -> Int
sumOfInts' l = sumOfInts'' l 0
    where
        sumOfInts'' :: [Int] -> Int -> Int
        sumOfInts'' [] acc =acc
        sumOfInts'' (h:t) acc = sumOfInts'' t (acc + h) --recursion

data Dumpable = Examples
data Command = Dump Dumpable | Sum [Int]

instance Show Dumpable where --interfeisas
    show :: Dumpable -> String
    show Examples = "examples" 

instance Show Command where
    show :: Command -> String
    show (Dump d) = "dump " ++ show d
    show (Sum is) = "sum_of " ++ show is ++ " is " ++ show (sumOfInts' is)

class FuzzyAdd a where --klase
    (~+~) :: a -> a -> a

a1 :: Int
a1 = 42

a2 :: Int
a2 = 1

instance FuzzyAdd Int where
    (~+~) :: Int -> Int -> Int
    (~+~) a b = a + b - 1

--RECORDS

--adts

safeHead :: [a] ->  Maybe a
safeHead [] = Nothing
safeHead (h: _) = Just h

safeHeadDefault :: [a] -> a -> a
safeHeadDefault l d = 
    case safeHead l of
        Nothing -> d
        Just a -> a