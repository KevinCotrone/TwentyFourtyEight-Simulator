module Simulation.TwentyFourtyEight.Internal
    (
      mapWithIndex
      ,remJusts
      ,toLists
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V

toLists ::  M.Matrix (Maybe Int) -> [[Maybe Int]]
toLists matrix =
  if (0 == (M.nrows matrix))
    then []
    else (V.toList $ M.getRow 1 matrix):(toLists $ M.submatrix 2 (M.nrows matrix) 1 (M.ncols matrix) matrix)

-- | Map a function over a 
--list and give the function the index of the list
mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex _ [] = []
mapWithIndex f xs = zipWith f [0..] xs

remJusts :: [(Int, Int, Maybe Int)] -> [(Int, Int)]
remJusts = map (\(a,b,_) -> (a,b))