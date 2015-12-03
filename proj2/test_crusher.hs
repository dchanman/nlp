module Main where

import Test.HUnit
import Crusher

-- Test piece counting
test_countPiecesW_1 = TestCase (assertEqual "Count pieces White" 1 (countPiecesW (sTrToBoard "-W-----") 0))
test_countPiecesW_2 = TestCase (assertEqual "Count pieces White" 2 (countPiecesW (sTrToBoard "WW--B-B") 0))
test_countPiecesW_3 = TestCase (assertEqual "Count pieces White" 7 (countPiecesW (sTrToBoard "WWWWWWW") 0))
test_countPiecesW_4 = TestCase (assertEqual "Count pieces White" 0 (countPiecesW (sTrToBoard "-------") 0))
test_countPiecesW_5 = TestCase (assertEqual "Count pieces White" 0 (countPiecesW (sTrToBoard "--B-B--") 0))
test_countPiecesW_6 = TestCase (assertEqual "Count pieces White" 2 (countPiecesW (sTrToBoard "W--B--W") 0))

test_countPiecesB_1 = TestCase (assertEqual "Count pieces Black" 1 (countPiecesB (sTrToBoard "-B-----") 0))
test_countPiecesB_2 = TestCase (assertEqual "Count pieces Black" 2 (countPiecesB (sTrToBoard "BB--W-W") 0))
test_countPiecesB_3 = TestCase (assertEqual "Count pieces Black" 7 (countPiecesB (sTrToBoard "BBBBBBB") 0))
test_countPiecesB_4 = TestCase (assertEqual "Count pieces Black" 0 (countPiecesB (sTrToBoard "-------") 0))
test_countPiecesB_5 = TestCase (assertEqual "Count pieces Black" 0 (countPiecesB (sTrToBoard "--W-W--") 0))
test_countPiecesB_6 = TestCase (assertEqual "Count pieces Black" 2 (countPiecesB (sTrToBoard "B--W--B") 0))

tests_count_pieces = TestList [
	TestLabel "Count White Pieces" test_countPiecesW_1,
	TestLabel "Count White Pieces" test_countPiecesW_2,
	TestLabel "Count White Pieces" test_countPiecesW_3,
	TestLabel "Count White Pieces" test_countPiecesW_4,
	TestLabel "Count White Pieces" test_countPiecesW_5,
	TestLabel "Count White Pieces" test_countPiecesW_6,
	TestLabel "Count White Pieces" test_countPiecesB_1,
	TestLabel "Count White Pieces" test_countPiecesB_2,
	TestLabel "Count White Pieces" test_countPiecesB_3,
	TestLabel "Count White Pieces" test_countPiecesB_4,
	TestLabel "Count White Pieces" test_countPiecesB_5,
	TestLabel "Count White Pieces" test_countPiecesB_6
	]			

main = do
	runTestTT tests_count_pieces

