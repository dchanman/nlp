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

-- Test generateSlides/Leaps

test_inSomeOrder_1 = TestCase (assertEqual "Increasing Sequence" True (inSomeOrder 1 2 3))
test_inSomeOrder_2 = TestCase (assertEqual "Increasing Sequence" True (inSomeOrder 0 1 2))
test_inSomeOrder_3 = TestCase (assertEqual "Increasing Sequence" True (inSomeOrder 2 4 6))
test_inSomeOrder_4 = TestCase (assertEqual "Increasing Sequence" True (inSomeOrder (-1) 0 1))
test_inSomeOrder_5 = TestCase (assertEqual "Decreasing Sequence" True (inSomeOrder 3 2 1))
test_inSomeOrder_6 = TestCase (assertEqual "Decreasing Sequence" True (inSomeOrder 2 1 0))
test_inSomeOrder_7 = TestCase (assertEqual "Decreasing Sequence" True (inSomeOrder 2 0 (-2)))
test_inSomeOrder_8 = TestCase (assertEqual "Decreasing Sequence" True (inSomeOrder 7 2 (-8)))
test_inSomeOrder_9 = TestCase (assertEqual "Bad Sequence" False (inSomeOrder 0 0 0))
test_inSomeOrder_10 = TestCase (assertEqual "Bad Sequence" False (inSomeOrder 1 1 1))
test_inSomeOrder_11 = TestCase (assertEqual "Bad Sequence" False (inSomeOrder 2 0 1))
test_inSomeOrder_12 = TestCase (assertEqual "Bad Sequence" False (inSomeOrder 3 (-2) 1))

tests_inSomeOrder = TestList [
	TestLabel "Check ordered" test_inSomeOrder_1,
	TestLabel "Check ordered" test_inSomeOrder_2,
	TestLabel "Check ordered" test_inSomeOrder_3,
	TestLabel "Check ordered" test_inSomeOrder_4,
	TestLabel "Check ordered" test_inSomeOrder_5,
	TestLabel "Check ordered" test_inSomeOrder_6,
	TestLabel "Check ordered" test_inSomeOrder_7,
	TestLabel "Check ordered" test_inSomeOrder_8,
	TestLabel "Check ordered" test_inSomeOrder_9,
	TestLabel "Check ordered" test_inSomeOrder_10,
	TestLabel "Check ordered" test_inSomeOrder_11,
	TestLabel "Check ordered" test_inSomeOrder_12
	]


main = do
	runTestTT tests_count_pieces;
	runTestTT tests_inSomeOrder;

