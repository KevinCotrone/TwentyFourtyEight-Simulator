module Main where


import Data.Maybe
import qualified Data.Matrix as M
import qualified Data.Set as S
import Data.Maybe
import Simulation.TwentyFourtyEight
import Test.Hspec


main :: IO ()
main = do
    (score, highestTile) <- runGameM strategy'
    scores <- runTimes strategy' 10000
    let highest = maximum $ scnds scores
    putStrLn $ "The score was: " ++ (show score)
    putStrLn $ "The highest tile was" ++ (show highest)

runTimes :: (M.Matrix (Maybe Int) -> IO MoveDirection) -> Int -> IO [(Int, Int)]
runTimes strat count = sequence $ runTimes' strat count

runTimes' :: (M.Matrix (Maybe Int) -> IO MoveDirection) -> Int -> [IO (Int,Int)]
runTimes' strat 0 = []
runTimes' strat x = (runGameM strat):(runTimes' strat (x-1))

scnds :: [(a,b)] -> [b]
scnds [] = []
scnds ((_,b):xs) = b:(scnds xs)

strategy :: M.Matrix (Maybe Int) -> IO MoveDirection
strategy board = do
  putStrLn . M.prettyMatrix $ board
  let directions = possibleDirections board
  putStrLn . show $ directions
  if (directions == S.empty)
    then return MoveUp
    else if ((S.size directions) == 1)
          then return $  S.elemAt 0 directions
          else return $  S.elemAt 1 directions

strategy' :: M.Matrix (Maybe Int) -> IO MoveDirection
strategy' board = do
  --_ <- getLine
  --putStrLn "Matrix2:"
  --putStrLn . M.prettyMatrix $ board
  let shiftedDown = shiftDown board
  case ((length . getEmpty $ shiftedDown) == (length . getEmpty $ board)) of
    True  -> do
      let shiftedRight = shiftRight board
      case ((length . getEmpty $ shiftedRight) == (length . getEmpty $ board)) of
        True  -> do
          let directions = possibleDirections board
          case (MoveDown `S.member` directions) of
            True  -> do
              --putStrLn "Moving Down"
              return MoveDown
            False -> do
              case (MoveRight `S.member` directions) of
                True -> do
                  --putStrLn "Moving Right"
                  return MoveRight
                False -> do
                  case (MoveLeft `S.member` directions) of
                    True  -> return MoveLeft
                    False -> do
                      --putStrLn "Moving up :("
                      return MoveUp
        False -> do
              --putStrLn "Moving Good Right"
              return MoveRight
    False -> do
              --putStrLn "Moving Good Down"
              return MoveDown

strat :: M.Matrix (Maybe Int) -> IO MoveDirection
strat board = do
  putStrLn "Matrix2:"
  putStrLn . M.prettyMatrix $ board
  let directions = possibleDirections board
  if (MoveDown `S.member` directions)
    then return MoveDown
    else if (MoveRight `S.member` directions)
      then return MoveRight
      else if (MoveLeft `S.member` directions)
        then return MoveLeft
        else return MoveUp
