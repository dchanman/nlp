module Crusher where

-- CPSC 312 - Project 2
-- by Khurram Ali Jaffery

-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--		 W is a piece of the White player
--		 B is a piece of the Black player
--


data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements 
-- representing what a point is occupied by
-- where the first element represents a piece 
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate 
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
--
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--		 newBoard is the next board to add to the tree
-- 		 seenBoards is the updated history to avoid possible future trouble boards
-- 		 cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
-- 		 board is the game state at that node
-- 		 nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show, Eq)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move over
--		 the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
-- 		 the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--		 is that a jump can be reduced to a move as in effect 
--		 nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- Some test results to see what functions are producing
--
--run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
--grid0 = generateGrid 3 2 4 []
--slides0 = generateSlides grid0 3
--jumps0 = generateLeaps grid0 3
--board0 = sTrToBoard "WWW-WW-------BB-BBB"
--newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
--tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
--heuristic0 = boardEvaluator W [] 3


--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of
-- search tree, the size of the provide boards, and produces the
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) p d n = (boardToStr nextBoard):(current:old)
	where 
		board = strToBoard current
		history = map strToBoard old
    	grid = generateGrid n (n - 1) (2 * (n - 1)) []
  		slides = generateSlides grid
  		jumps = generateLeaps grid
  		player = getPlayer p
  	    nextBoard = stateSearch board history grid slides jumps player d n

-- Helper function for crusher to change type of player input from Char to Piece
getPlayer :: Char -> Piece
getPlayer p 
	| p == 'W'  = W
	| p == 'B'  = B
	
--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
--

gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n
	| elem board history = True
	| (playerCounter W board) < n = True
	| (playerCounter B board) < n = True
	| otherwise = False


--
-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
-- 	     [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
	where 
		check 'W' = W
		check 'B' = B
		check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B] 
-- 	     to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board 
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
	where 
		check W = 'W'
		check B = 'B'
		check D = '-'

--
-- boardToState
--
-- This function consumes a board and a grid to convert them into a state.
--
-- Arguments:
-- -- board: the Board to convert into a State
-- -- grid: the Grid to convert into a State
--
-- Returns: The State corresponding to the board and the grid
--
boardToState :: Board -> Grid -> State
boardToState [] grid = []
boardToState board [] = []
boardToState (boardHead:boardTail) (gridHead:gridTail) = (boardHead,gridHead) : (boardToState boardTail gridTail)

stateToBoard :: State -> Board
stateToBoard state = [x | (x,_) <- state]
--stateToBoard ((piece,_):stateTail) = piece : (stateToBoard stateTail)

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid 
--		   initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--		 [                (-2, 2),        ( 0, 2),        ( 2, 2)
--		          (-3, 1),        (-1, 1),        ( 1, 1),        ( 3, 1),
--		  (-4, 0),        (-2, 0),        ( 0, 0),        ( 2, 0),        ( 4, 0),
--		          (-3,-1),        (-1,-1),        ( 1,-1),        (3,-1),
--		                  (-2,-2),        ( 0,-2),        ( 2,-2)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
	| n3 == -1		= acc
	| otherwise 	= generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
		where
			row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
			nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible slides is only generated once; and when 
-- 		 generating next moves, the program decides which slides out of 
--		 all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
--

generateSlides :: Grid -> [Slide]
generateSlides b = [((x1,y1),(x2,y2)) |
	(x1,y1) <- b,
	(x2,y2) <- b,
	-- If we don't switch rows, its an adjacent x spot (x +- 2)
	((y1 == y2) && (abs (x1 - x2) == 2)) ||
	-- If we do switch rows, (x +- 1)
	((abs (y1 - y2) == 1) && (abs (x1-x2) == 1))
	]
	

--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible leaps is only generated once; and when 
-- 		 generating next moves, the program decides which leaps out of 
--		 all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

inSomeOrder :: Int -> Int -> Int -> Bool
inSomeOrder a b c = (a > b && b > c) || (a < b && b < c)

generateLeaps :: Grid -> [Jump]
generateLeaps b = [((x1,y1),(x2,y2),(x3,y3)) |
	(x1,y1) <- b,
	(x2,y2) <- b,
	(x3,y3) <- b,
	-- Jumping along a row
	(y1 == y2 && y2 == y3 && (inSomeOrder x1 x2 x3)) ||
	-- Jumping between rows
	((inSomeOrder y1 y2 y3) && (inSomeOrder x1 x2 x3))
	]

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the 
-- board, else generate a search tree till the specified depth and apply 
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over, 
--          otherwise produces the next best board
--

stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth n -- To Be Completed
	| gameOver board history n = board
	| depth == 0 = board
	| otherwise = minimax (generateTree board history grid slides jumps player depth n) (boardEvaluator player history n)


--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n
		| depth == 0 = Node depth board ([])
		| gameOver board history n = (Node depth board [])
		| otherwise = Node depth board allTrees
		where
			nextBoard = generateNewStates board history grid slides jumps player
			nextTrees new_board = generateTree new_board (board:history) grid slides jumps player (depth - 1) n
			allTrees = map nextTrees nextBoard
--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player = allPossibleNewBoards where
		-- convert board into a state so we can apply moves to it
		boardAsState = (boardToState board grid);
		-- apply moves to board to retrieve all possible boards
		allPossibleStates = map (applyMoveToState boardAsState) (moveGenerator boardAsState slides jumps player);
		-- convert all possible boards back into states so we can check history
		allPossibleBoards = map stateToBoard allPossibleStates;
		-- filter by history
		allPossibleNewBoards = [newBoard | newBoard <- allPossibleBoards, (elem newBoard history) == False]

-- Helper function, retrieves the Piece at a specified Point in a State
getPieceAtPoint :: State -> Point -> Piece
getPieceAtPoint [] point = error ("Point " ++ show point ++ " does not exist in the state provided")
getPieceAtPoint ((piece,statePoint) : stateTail) point
	| statePoint == point = piece
	| otherwise = getPieceAtPoint stateTail point

-- Helper function, returns a State with a new Piece at a specified Point
setPieceAtPoint :: State -> Piece -> Point -> State
setPieceAtPoint [] _piece point = error ("Point " ++ show point ++ " does not exist in the state provided")
setPieceAtPoint ((statePiece,statePoint) : stateTail) piece point
	| statePoint == point = (piece, point) : stateTail
	| otherwise = (statePiece,statePoint) : (setPieceAtPoint stateTail piece point)

-- Helper function, consumes a State and a Move, produces a new State that
-- is the result of performing the move in the State
applyMoveToState ::  State -> Move -> State
applyMoveToState state (point1,point2) = newState where
	piece = getPieceAtPoint state point1;
	transitionState = setPieceAtPoint state piece point2;
	newState = setPieceAtPoint transitionState D point1
--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps, 
-- a list of possible slides and a player from whose perspective 
-- to generate moves, to check which of these jumps and slides 
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--		 type State, for our purposes it is zipping the board and the
--		 grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--

-- Helper function to help merge two lists
merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x : merge xs ys

-- Helper function to generate slide moves
slideMoves :: State -> [Slide] -> Piece -> [Move]
slideMoves state slides player = [(orig_point, dest_point) |
	-- orig_points are the points occupied by the player's pieces
	(piece, orig_point) <- state,
	piece == player,

	-- dest_points are unoccupied
	(empty, dest_point) <- state,
	empty == D,

	-- orig_point and dest_points must be reachable by a slide
	elem (orig_point, dest_point) slides
	]

-- Helper function to generate jump moves
jumpMoves :: State -> [Jump] -> Piece -> [Move]
jumpMoves state jumps player = [(orig_point, dest_point) |
	-- orig_points are points occupied by the player's pieces
	(piece, orig_point) <- state,
	piece == player,

	-- second_points must be allies
	(ally, second_point) <- state,
	ally == player,
	orig_point /= second_point,

	-- don't crush friends
	(target, dest_point) <- state,
	target /= player,

	-- make sure its a legal jump
	elem (orig_point, second_point, dest_point) jumps
	]

moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player =
	merge (slideMoves state slides player) (jumpMoves state jumps player)

--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
boardEvaluator player history n board myTurn = abs((playerCounter player board) - (playerCounter (negatePlayer player) board));

-- I wrote this helper to help with the above code.
-- It just take the current player and returns the other player (aka opponent)
negatePlayer :: Piece -> Piece
negatePlayer player
	| player == W = B
	| player == B = W

-- Helper function for counting number of W or B in a list
playerCounter :: Piece -> Board -> Int
playerCounter player board
	| player == W 	= countPiecesW board 0
	| player == B	= countPiecesB board 0
	
	
countPiecesW :: Board -> Int -> Int
countPiecesW board acc 
	| null board			= acc			
	| (head board) == W		= countPiecesW (tail board) (acc + 1)
	| otherwise 			= countPiecesW (tail board) acc
	
countPiecesB :: Board -> Int -> Int
countPiecesB board acc 
	| null board			= acc			
	| (head board) == B		= countPiecesB (tail board) (acc + 1)
	| otherwise 			= countPiecesB (tail board) acc	
	
	
--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree, 
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a partially evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--

minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node d b children) heuristic = findChild (minimax' (Node d b children) heuristic True) 
   [(board x, heuristic (board x) True) | x <- children]

-- A helper function to find the board that corresponds to the correct minimax value reported
findChild :: Int -> [(Board,Int)] -> Board
findChild val (t:ts) 
	| snd t == val  = fst t
	| otherwise     = findChild val ts

--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes 
-- a search tree, an appropriate heuristic to apply to the leaf nodes of 
-- the tree, and based on whether it would have been the maximizing 
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a partially evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
-- 				 or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--

minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
minimax' (Node _ b []) heuristic maxPlayer = heuristic b True
minimax' (Node _ b children) heuristic maxPlayer
	| maxPlayer  = maximum ([minimax' x heuristic False | x <- children])
	| otherwise  = minimum ([minimax' x heuristic True | x <- children])
