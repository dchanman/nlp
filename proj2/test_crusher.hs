module Main where

import Test.HUnit
import Crusher
import Data.List

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
	test_countPiecesW_1,
	test_countPiecesW_2,
	test_countPiecesW_3,
	test_countPiecesW_4,
	test_countPiecesW_5,
	test_countPiecesW_6,
	test_countPiecesB_1,
	test_countPiecesB_2,
	test_countPiecesB_3,
	test_countPiecesB_4,
	test_countPiecesB_5,
	test_countPiecesB_6
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
	test_inSomeOrder_1,
	test_inSomeOrder_2,
	test_inSomeOrder_3,
	test_inSomeOrder_4,
	test_inSomeOrder_5,
	test_inSomeOrder_6,
	test_inSomeOrder_7,
	test_inSomeOrder_8,
	test_inSomeOrder_9,
	test_inSomeOrder_10,
	test_inSomeOrder_11,
	test_inSomeOrder_12
	]

-- Helper test function, checks to make sure two lists contain identical members, except maybe
-- in a different order.
helper_listsContainSameUniqueMembers a b = (sort a) == (sort b)

-- Sample board we will test with
--					 (-1,-1)   ( 1,-1)
--               (-2, 0)  ( 0, 0)  ( 2, 0)
--                   (-1, 1)   ( 1, 1)

sample_generateSlides = generateSlides [(-1,-1),(1,-1),(-2,0),(0,0),(2,0),(-1,1),(1,1)]
test_generateSlides_1 = TestCase (assertBool "Corner slides" (elem ((-1,-1),(1,-1)) sample_generateSlides))
test_generateSlides_2 = TestCase (assertBool "Corner slides" (elem ((-1,-1),(0,0)) sample_generateSlides))
test_generateSlides_3 = TestCase (assertBool "Corner slides" (elem ((-1,-1),(-2,0)) sample_generateSlides))
test_generateSlides_all = TestCase (assertBool "All"
	(helper_listsContainSameUniqueMembers sample_generateSlides
		[
		((-1,-1),(1,-1)),((-1,-1),(-2,0)),((-1,-1),(0,0)),
		((1,-1),(-1,-1)),((1,-1),(0,0)),((1,-1),(2,0)),
		((-2,0),(-1,-1)),((-2,0),(0,0)),((-2,0),(-1,1)),
		((0,0),(-1,-1)),((0,0),(1,-1)),((0,0),(-2,0)),((0,0),(2,0)),((0,0),(-1,1)),((0,0),(1,1)),
		((2,0),(1,-1)),((2,0),(0,0)),((2,0),(1,1)),
		((-1,1),(-2,0)),((-1,1),(0,0)),((-1,1),(1,1)),
		((1,1),(0,0)),((1,1),(2,0)),((1,1),(-1,1))
		]))

sample_generateLeaps = generateLeaps [(-1,-1),(1,-1),(-2,0),(0,0),(2,0),(-1,1),(1,1)]
test_generateLeaps_all = TestCase (assertBool "All"
	(helper_listsContainSameUniqueMembers sample_generateLeaps
		[
		((-1,-1),(0,0),(1,1)),
		((1,-1),(0,0),(-1,1)),
		((-2,0),(0,0),(2,0)),
		((2,0),(0,0),(-2,0)),
		((-1,1),(0,0),(1,-1)),
		((1,1),(0,0),(-1,-1))
		]))

tests_generateSlidesLeaps = TestList [
	test_generateSlides_1,
	test_generateSlides_2,
	test_generateSlides_3,
	test_generateSlides_all,
	test_generateLeaps_all
	]

-------------------------------------------------------------------------------
-- Test slideMoves, jumpMoves, moveGenerator

test_slideMoves_1 = TestCase (assertBool "Cannot slide over friends"
	(helper_listsContainSameUniqueMembers
		[] -- Expected outcome
		(slideMoves
		[(W,(0,0)), (W,(0,2))] -- The state
		[((0,0),(0,2)), ((0,2),(0,0))] -- The legal moves
		W) -- The player
		)
	)

test_slideMoves_2 = TestCase (assertBool "Cannot slide over enemies"
	(helper_listsContainSameUniqueMembers
		[] -- Expected outcome
		(slideMoves
		[(W,(0,0)), (B,(0,2))] -- The state
		[((0,0),(0,2)), ((0,2),(0,0))] -- The legal moves
		W) -- The player
		)
	)

test_slideMoves_3 = TestCase (assertBool "Slide into empty is possible"
	(helper_listsContainSameUniqueMembers
		[((0,0),(0,2))] -- Expected outcome
		(slideMoves
			[(W,(0,0)), (D,(0,2))] -- The state
			[((0,0),(0,2)), ((0,2),(0,0))] -- The legal moves
			W) -- The player
		)
	)

test_slideMoves_4 = TestCase (assertBool "Multiple moves possible"
	(helper_listsContainSameUniqueMembers
		[((0,0),(0,2)), ((0,4),(0,2))] -- Expected outcome
		(slideMoves
			[(W,(0,0)), (D,(0,2)), (W,(0,4))] -- The state
			[((0,0),(0,2)), ((0,2),(0,0)), ((0,2),(0,4)), ((0,4),(0,2))] -- The legal moves
			W) -- The player
		)
	)

test_slideMoves_5 = TestCase (assertBool "Multiple player pieces, only one slide possible"
	(helper_listsContainSameUniqueMembers
		[((0,2),(0,4))] -- Expected outcome
		(slideMoves
			[(W,(0,0)), (W,(0,2)), (D,(0,4))] -- The state
			[((0,0),(0,2)), ((0,2),(0,0)), ((0,2),(0,4)), ((0,4),(0,2))] -- The legal moves
			W) -- The player
		)
	)

test_jumpMoves_1 = TestCase (assertBool "Need friends to jump"
	(helper_listsContainSameUniqueMembers
		[] -- Expected outcome
		(jumpMoves
			[(W,(0,0)), (D,(0,2)), (D,(0,4))] -- The state
			[((0,0),(0,2),(0,4)), ((0,4),(0,2),(0,0))] -- The legal moves
			W) -- The player
		)
	)

test_jumpMoves_2 = TestCase (assertBool "Can't jump over enemies"
	(helper_listsContainSameUniqueMembers
		[] -- Expected outcome
		(jumpMoves
			[(W,(0,0)), (B,(0,2)), (D,(0,4))] -- The state
			[((0,0),(0,2),(0,4)), ((0,4),(0,2),(0,0))] -- The legal moves
			W) -- The player
		)
	)

test_jumpMoves_3 = TestCase (assertBool "Can't squish friends"
	(helper_listsContainSameUniqueMembers
		[] -- Expected outcome
		(jumpMoves
			[(W,(0,0)), (W,(0,2)), (W,(0,4))] -- The state
			[((0,0),(0,2),(0,4)), ((0,4),(0,2),(0,0))] -- The legal moves
			W) -- The player
		)
	)

test_jumpMoves_4 = TestCase (assertBool "Can jump to empty"
	(helper_listsContainSameUniqueMembers
		[((0,0),(0,4))] -- Expected outcome
		(jumpMoves
			[(W,(0,0)), (W,(0,2)), (D,(0,4))] -- The state
			[((0,0),(0,2),(0,4)), ((0,4),(0,2),(0,0))] -- The legal moves
			W) -- The player
		)
	)

test_jumpMoves_5 = TestCase (assertBool "CRUSH"
	(helper_listsContainSameUniqueMembers
		[((0,0),(0,4))] -- Expected outcome
		(jumpMoves
			[(W,(0,0)), (W,(0,2)), (B,(0,4))] -- The state
			[((0,0),(0,2),(0,4)), ((0,4),(0,2),(0,0))] -- The legal moves
			W) -- The player
		)
	)

test_jumpMoves_6 = TestCase (assertBool "Multiple CRUSH options"
	(helper_listsContainSameUniqueMembers
		[((0,0),(0,4)), ((0,8),(0,4))] -- Expected outcome
		(jumpMoves
			[(W,(0,0)), (W,(0,2)), (B,(0,4)), (W,(0,6)), (W,(0,8))] -- The state
			[((0,0),(0,2),(0,4)), ((0,4),(0,2),(0,0)), ((0,8),(0,6),(0,4)), ((0,4),(0,6),(0,8))] -- The legal moves
			W) -- The player
		)
	)

test_moveGenerator_1 = TestCase (assertBool "Slide and jump"
	(helper_listsContainSameUniqueMembers
		[((0,2),(0,0)), ((0,2),(0,6)), ((0,4),(0,0))] -- Expected outcome
		(moveGenerator
			[(D,(0,0)), (W,(0,2)), (W,(0,4)), (B,(0,6))] -- The state
			[((0,0),(0,2)),((0,2),(0,0)),((0,2),(0,4)),((0,4),(0,2)),((0,4),(0,6)),((0,6),(0,4))] -- The legal slides
			[((0,0),(0,2),(0,4)),((0,4),(0,2),(0,0)),((0,2),(0,4),(0,6)),((0,6),(0,4),(0,2))] -- The legal jumps
			W) -- The player
		)
	)

tests_slideMovesJumpsGenerator = TestList [
	test_slideMoves_1,
	test_slideMoves_2,
	test_slideMoves_3,
	test_slideMoves_4,
	test_slideMoves_5,
	test_jumpMoves_1,
	test_jumpMoves_2,
	test_jumpMoves_3,
	test_jumpMoves_4,
	test_jumpMoves_5,
	test_jumpMoves_6,
	test_moveGenerator_1
	]

-------------------------------------------------------------------------------
-- Test boardToState

test_boardToState_1 = TestCase (assertEqual "Convert board to state"
	[(W,(0,0)),(W,(0,1))]
	(boardToState [W,W] [(0,0),(0,1)])
	)

test_boardToState_2 = TestCase (assertEqual "Convert board to state"
	[(W,(0,0)),(D,(0,1)),(B,(0,2))]
	(boardToState [W,D,B] [(0,0),(0,1),(0,2)])
	)

tests_boardToState = TestList [
	test_boardToState_1,
	test_boardToState_2
	]

	-------------------------------------------------------------------------------
	-- Test getPieceAtPoint

test_getPieceAtPoint_1 = TestCase (assertEqual "Get W piece"
	W
	(getPieceAtPoint [(W,(1,1)),(B,(2,1))] (1,1))
	)

test_getPieceAtPoint_2 = TestCase (assertEqual "Get B piece"
	B
	(getPieceAtPoint [(W,(1,1)),(B,(2,1))] (2,1))
	)

test_getPieceAtPoint_3 = TestCase (assertEqual "Get D piece"
	D
	(getPieceAtPoint [(W,(1,1)),(D,(3,2)),(B,(2,1))] (3,2))
	)

test_setPieceAtPoint_1 = TestCase (assertEqual "Set B piece"
	[(W,(1,1)),(B,(3,2)),(B,(2,1))]
	(setPieceAtPoint [(W,(1,1)),(D,(3,2)),(B,(2,1))] B (3,2))
	)

test_setPieceAtPoint_2 = TestCase (assertEqual "Set D piece"
	[(D,(1,1)),(D,(3,2)),(B,(2,1))]
	(setPieceAtPoint [(W,(1,1)),(D,(3,2)),(B,(2,1))] D (1,1))
	)

test_setPieceAtPoint_3 = TestCase (assertEqual "Set W piece"
	[(W,(1,1)),(D,(3,2)),(W,(2,1))]
	(setPieceAtPoint [(W,(1,1)),(D,(3,2)),(B,(2,1))] W (2,1))
	)

test_applyMoveToState = TestCase (assertEqual "Move"
	[(D,(1,1)),(W,(3,2)),(B,(2,1))]
	(applyMoveToState [(W,(1,1)),(D,(3,2)),(B,(2,1))] ((1,1),(3,2)))
	)

tests_getPieceAtPoint = TestList [
	test_getPieceAtPoint_1,
	test_getPieceAtPoint_2,
	test_getPieceAtPoint_3,
	test_setPieceAtPoint_1,
	test_setPieceAtPoint_2,
	test_setPieceAtPoint_3,
	test_applyMoveToState
	]

sampleMiniGrid = [(-2,0),(0,0),(2,0)]
test_generateNewStates_1 = TestCase (assertEqual "Generate New States"
	[(sTrToBoard "-W-")]
	(generateNewStates
		(sTrToBoard "W--") -- the current state
		[] -- history
		sampleMiniGrid -- grid
		(generateSlides sampleMiniGrid) -- slides
		(generateLeaps sampleMiniGrid) -- jumps
		W -- the player
		)
	)

test_generateNewStates_2 = TestCase (assertEqual "Generate New States, filter history"
	[]
	(generateNewStates
		(sTrToBoard "W--") -- the current state
		[(sTrToBoard "-W-")] -- history
		sampleMiniGrid -- grid
		(generateSlides sampleMiniGrid) -- slides
		(generateLeaps sampleMiniGrid) -- jumps
		W -- the player
		)
	)

sampleSmallGrid = [(-1,-1),(1,-1),(-2,0),(0,0),(2,0),(-1,1),(1,1)]
test_generateNewStates_3 = TestCase (assertEqual "Generate New States, small grid"
	-- Expected Output
	[(sTrToBoard "W------"),
		(sTrToBoard "-W-----"),
		(sTrToBoard "--W----"),
		(sTrToBoard "----W--"),
		(sTrToBoard "-----W-"),
		(sTrToBoard "------W")
		]
	-- Actual Output
	(generateNewStates
		(sTrToBoard "---W---") -- the current state
		[] -- history
		sampleSmallGrid -- grid
		(generateSlides sampleSmallGrid) -- slides
		(generateLeaps sampleSmallGrid) -- jumps
		W -- the player
		)
	)

test_generateNewStates_4 = TestCase (assertEqual "Generate New States, small grid, some history"
	-- Expected Output
	[(sTrToBoard "W------"),
		(sTrToBoard "-W-----"),
		(sTrToBoard "-----W-"),
		(sTrToBoard "------W")
		]
	-- Actual Output
	(generateNewStates
		(sTrToBoard "---W---") -- the current state
		[(sTrToBoard "--W----"),(sTrToBoard "----W--")] -- history
		sampleSmallGrid -- grid
		(generateSlides sampleSmallGrid) -- slides
		(generateLeaps sampleSmallGrid) -- jumps
		W -- the player
		)
	)

test_generateNewStates_5 = TestCase (assertEqual "Generate New States, small grid, some blocks"
	-- Expected Output
	[(sTrToBoard "B--W---"),
		(sTrToBoard "---W-B-")
		]
	-- Actual Output
	(generateNewStates
		(sTrToBoard "--BW---") -- the current state
		[] -- history
		sampleSmallGrid -- grid
		(generateSlides sampleSmallGrid) -- slides
		(generateLeaps sampleSmallGrid) -- jumps
		B -- the player
		)
	)

test_generateNewStates_6 = TestCase (assertEqual "Generate New States, small grid, some leaps"
	-- Expected Output
	[(sTrToBoard "B--B---"),
		(sTrToBoard "---B-B-"),
		(sTrToBoard "B-B----"),
		(sTrToBoard "-BB----"),
		(sTrToBoard "--B-B--"),
		(sTrToBoard "--B--B-"),
		(sTrToBoard "--B---B"),
		(sTrToBoard "---BB--")
		]
	-- Actual Output
	(generateNewStates
		(sTrToBoard "--BB---") -- the current state
		[] -- history
		sampleSmallGrid -- grid
		(generateSlides sampleSmallGrid) -- slides
		(generateLeaps sampleSmallGrid) -- jumps
		B -- the player
		)
	)

test_generateNewStates_7 = TestCase (assertEqual "Generate New States, small grid, some crushing"
	-- Expected Output
	[(sTrToBoard "B--BW--"),
		(sTrToBoard "---BWB-"),
		(sTrToBoard "B-B-W--"),
		(sTrToBoard "-BB-W--"),
		(sTrToBoard "--B-WB-"),
		(sTrToBoard "--B-W-B"),
		(sTrToBoard "---BB--")
		]
	-- Actual Output
	(generateNewStates
		(sTrToBoard "--BBW--") -- the current state
		[] -- history
		sampleSmallGrid -- grid
		(generateSlides sampleSmallGrid) -- slides
		(generateLeaps sampleSmallGrid) -- jumps
		B -- the player
		)
	)

test_generateNewStates_8 = TestCase (assertEqual "Generate New States, small grid, some crushing, some history"
	-- Expected Output
	[
		(sTrToBoard "---BWB-"),
		(sTrToBoard "B-B-W--"),
		(sTrToBoard "-BB-W--"),
		(sTrToBoard "---BB--")
		]
	-- Actual Output
	(generateNewStates
		(sTrToBoard "--BBW--") -- the current state
		[(sTrToBoard "B--BW--"),(sTrToBoard "--B-WB-"),(sTrToBoard "--B-W-B")] -- history
		sampleSmallGrid -- grid
		(generateSlides sampleSmallGrid) -- slides
		(generateLeaps sampleSmallGrid) -- jumps
		B -- the player
		)
	)

tests_generateNewStates = TestList [
	test_generateNewStates_1,
	test_generateNewStates_2,
	test_generateNewStates_3,
	test_generateNewStates_4,
	test_generateNewStates_5,
	test_generateNewStates_6,
	test_generateNewStates_7,
	test_generateNewStates_8
	]

main = do
	runTestTT tests_count_pieces;
	runTestTT tests_inSomeOrder;
	runTestTT tests_generateSlidesLeaps;
	runTestTT tests_slideMovesJumpsGenerator;
	runTestTT tests_boardToState;
	runTestTT tests_getPieceAtPoint;
	runTestTT tests_generateNewStates
