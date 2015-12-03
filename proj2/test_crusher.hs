module Main where

-- Imports
import Test.QuickCheck
import Crusher

-- Hey guys, here's a sample o
-- Sample function
greater_than x y = (x > y)
-- Sample test
prop_greater_than x y = (x > y) == greater_than x y

-------------------------------------------------------------------------------
-- main
-- 
-- Run your tests here using quickCheck:
main = do 
	quickCheck (prop_greater_than :: Int -> Int -> Bool);
	
