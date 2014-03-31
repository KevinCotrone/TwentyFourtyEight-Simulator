{-# LANGUAGE FlexibleInstances #-}
module Simulation.TwentyFourtyEight where

--Helping methods that I don't need to expose
import Simulation.TwentyFourtyEight.Internal

--Containers
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Vector as V

--General
import Control.Applicative
import Data.List
import Data.Maybe
import Foreign.Storable
import System.Random

data MoveDirection = MoveRight | MoveLeft | MoveUp | MoveDown deriving (Read, Show, Eq, Ord)

-- | Create a new board and start the recursive simulation of the board with a log
--runGameMLog :: (M.Matrix (Maybe Int) -> IO MoveDirection) -> IO (Int, Int,[M.Matrix (Maybe Int)])
--runGameMLog sim = do
--  let board = M.matrix 4 4 (\(_,_) -> Nothing) --Default board with nothing on it
--  (Just p1) <- randBoardPoint board                        --Random points and tile values
--  v1 <- randomTileValue
--  let board' = editBoard board p1 (Just v1)
--  (Just p2) <- randBoardPoint board'                       --Second randbom point and tile value
--  v2 <- randomTileValue
--  let board'' = editBoard board' p2 (Just v2)       --Final board with both points added
--  list <- runGameM' sim board''
--  return (0, maximum . listTiles $ (last list), list)   

---- | Recursively run the game with a log
--runGameMLog' :: (M.Matrix (Maybe Int) ->IO MoveDirection) -> M.Matrix (Maybe Int) -> IO [ (M.Matrix (Maybe Int))]
--runGameMLog' sim board = do
--  direction <- sim board
--  board' <- addRandom (shiftBoard direction board)
--  let direction = possibleDirections board
--  case ((S.size direction) == 0) of -- Not sure of a better way to do this (Possibly fix!)
--    True -> do
--      return [board]
--    False -> do 
--      l <- runGameMLog' sim board'
--      return board:l

-- | Create a new board and start the recursive simulation of the board
runGameM :: (M.Matrix (Maybe Int) -> IO MoveDirection) -> IO (Int, Int)
runGameM sim = do
  let board = M.matrix 4 4 (\(_,_) -> Nothing) --Default board with nothing on it
  (Just p1) <- randBoardPoint board                        --Random points and tile values
  v1 <- randomTileValue
  let board' = editBoard board p1 (Just v1)
  (Just p2) <- randBoardPoint board'                       --Second randbom point and tile value
  v2 <- randomTileValue
  let board'' = editBoard board' p2 (Just v2)       --Final board with both points added
  runGameM' sim board''                              -- recursively run the game


-- | Recursively run the game
runGameM' :: (M.Matrix (Maybe Int) ->IO MoveDirection) -> M.Matrix (Maybe Int) -> IO (Int, Int)
runGameM' sim board = do
  direction <- sim board
  board' <- addRandom (shiftBoard direction board)
  let direction = possibleDirections board
  case ((S.size direction) == 0) of -- Not sure of a better way to do this (Possibly fix!)
    True -> do
      return (0,(maximum . listTiles $ board)) --Game is over
    False -> do 
        runGameM' sim board'

possibleDirections :: (M.Matrix (Maybe Int)) -> S.Set MoveDirection
possibleDirections board = S.fromList . catMaybes $ map (possibleDirection board)  [MoveRight, MoveLeft, MoveUp, MoveDown] --Map over all move directions


possibleDirection :: (M.Matrix (Maybe Int)) -> MoveDirection -> (Maybe MoveDirection)
possibleDirection board move = 
  let board' = shiftBoard move board
  in
    if (board' == board)
      then Nothing
      else Just move


addRandom :: (M.Matrix (Maybe Int)) -> IO (M.Matrix (Maybe Int))
addRandom board = do
  position <- randBoardPoint board
  case position of
    (Nothing) -> return board
    (Just pos) -> do
                    val <- randomTileValue
                    --putStrLn ("Random: " ++ (show pos))
                    return $ editBoard board pos (Just val)

-- Edit a board item to make it the new item
editBoard :: (M.Matrix (Maybe Int)) -> (Int, Int) -> Maybe Int -> M.Matrix (Maybe Int)
editBoard board point item = M.setElem item point board



-- | Returns an open board position
randBoardPoint :: (M.Matrix (Maybe Int)) -> IO (Maybe (Int, Int))
randBoardPoint board = do
  --putStrLn "Matrix1:"
  --putStrLn . M.prettyMatrix $ board
  let flatened = flattenLists $ toLists board
  --putStrLn $ ("Flattened") ++  (show flatened)
  let list = justNothings flatened
  --putStrLn $  "empty: " ++ (show  list)
  case (length list) of
    0 -> return Nothing
    _ -> do
          rNum <- randomRIO (0, (length list) - 1)
          let x = (\(a,b,_) -> (a+1,b+1)) $ list !! (rNum)
          --putStrLn $ "Rand2" ++ (show x)
          return . Just $ x

-- | Just get the empty values
justNothings :: [(Int,Int,Maybe Int)] -> [(Int, Int, Maybe Int)]
justNothings [] = []
justNothings (val@(_,_,Nothing):xs) = val : (justNothings xs)
justNothings (x:xs) = justNothings xs


-- | Prints a board to the console
-- with 0 for empty tiles
printBoard :: (M.Matrix (Maybe Int)) -> IO ()
printBoard board = do
  let lists = toLists board
  mapM_ (putStrLn . show) lists

-- | 90% chance of generating a 2
--  10% chance of generating a 4
randomTileValue :: IO Int
randomTileValue = do
  r <- randomIO :: IO Double
  case (r < 0.9) of
    True  -> return 2
    False -> return 4


listTiles :: M.Matrix (Maybe Int) -> [Int]
listTiles = catMaybes . concat . toLists

-- | Get a list of all empty tile positions
getEmpty :: M.Matrix (Maybe Int) -> [(Int, Int, Maybe Int)]
getEmpty = justNothings . flattenLists . toLists

flattenLists :: [[Maybe Int]] -> [(Int, Int, Maybe Int)]
flattenLists [] = []
flattenLists xs = concat $ map formatFlatList $ mapWithIndex (\i a -> (i,getPositions i a)) xs

formatFlatList :: (Int, [(Int, Maybe Int)]) -> [(Int, Int, Maybe Int)]
formatFlatList (_,[]) = []
formatFlatList (r,((x,y):xs)) = (r,x,y):(formatFlatList (r,xs))


-- | Get the positions of a M.matrix row given it's row
getPositions :: Int -> [Maybe Int] -> [(Int, Maybe Int)]
getPositions row cols = mapWithIndex (\i a -> (i,a)) cols

shiftBoard :: MoveDirection -> M.Matrix (Maybe Int) -> (M.Matrix (Maybe Int))
shiftBoard MoveRight = shiftRight
shiftBoard MoveLeft = shiftLeft
shiftBoard MoveUp = shiftUp
shiftBoard MoveDown = shiftDown

-- | Shifts a board to the right
--  if the shift fails it returns Nothing
--  Also adds the new tile onto the board
shiftRight :: (M.Matrix (Maybe Int)) -> (M.Matrix (Maybe Int))
shiftRight board =
  M.fromLists $ final
  where size = M.nrows board
        lists = toLists board
        lists' = map catMaybes lists
        changedRows = listChangedRows lists'
        combined = map (combineRight) lists'
        final = map (addRightMaybes (M.ncols board)) combined :: [[Maybe Int]]

addRightMaybes :: Int -> [Int] -> [Maybe Int]
addRightMaybes size xs = 
    nList ++ (Just <$> xs)
    where
      nothings = repeat Nothing
      lSize = size - (length xs)
      nList = map getNothings $ zip [1..lSize] nothings
      getNothings (_,b) = b

-- | Combine the right shifted items a reversed combineLeft that is reversed
combineRight :: [Int] -> [Int]
combineRight = reverse . combineLeft . reverse

-- | Combine the left shifted items
combineLeft :: [Int] -> [Int]
combineLeft [] = []
combineLeft (x:y:xs) = 
  if x == y
    then (x+y) : combineLeft xs
    else x:combineLeft (y:xs)
combineLeft (x:xs) = x:combineLeft xs


-- | Returns a list of the rows that have changed
listChangedRows :: [[Int]] -> Int -> [Int]
listChangedRows xs size = catMaybes $ mapWithIndex (matchesSize size) xs
  where matchesSize size i xs = 
          if (length xs == size)
            then Nothing
            else Just i

-- | Shifts a board to the left
--  if the shift fails it returns Nothing
--  Also adds the new tile onto the board
shiftLeft :: (M.Matrix (Maybe Int)) -> (M.Matrix (Maybe Int))
shiftLeft board =
  M.fromLists $ final
  where size = M.nrows board
        lists = toLists board
        lists' = map catMaybes lists
        changedRows = listChangedRows lists'
        combined = map (combineLeft) lists'
        final = map (addLeftMaybes (M.ncols board)) combined :: [[Maybe Int]]

addLeftMaybes :: Int -> [Int] -> [Maybe Int]
addLeftMaybes size xs = 
    (Just <$> xs) ++ nList
    where
      nothings = repeat Nothing
      lSize = size - (length xs)
      nList = map getNothings $ zip [1..lSize] nothings
      getNothings (_,b) = b

-- | Shifts a board up
--  if the shift fails it returns Nothing
--  Also adds the new tile onto the board
--  This is a twice transposed left shift
shiftUp :: (M.Matrix (Maybe Int)) -> (M.Matrix (Maybe Int))
shiftUp board = M.transpose . shiftLeft . M.transpose $ board


-- | Shift a board down
--  if the shift fails it returns Nothing
--  Also adds the new tile onto the board
-- This is a twice transposed right shift
shiftDown :: (M.Matrix (Maybe Int)) -> (M.Matrix (Maybe Int))
shiftDown board = M.transpose . shiftRight . M.transpose $ board