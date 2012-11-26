module LifeGloss where

import Life (randomBoard, Cell(..), Board, mainCells, updateBoard, Coordinate)
import System.Random
import Graphics.Gloss
import Debug.Trace

displaySize :: Display -> (Int, Int)
displaySize (InWindow _ (x, y) _) = (x, y)
displaySize (FullScreen (x, y))   = (x, y)


cell2Picture :: Float -> Cell -> Picture
cell2Picture size c = cellColor c $ square size
    where
      cellColor Alive = Color $ greyN 0.3
      cellColor Dead  = Color orange

board2Picture :: Display -> Board -> Picture
board2Picture d b = Pictures $ map (\(pt, c) -> Translate (fst pt) (snd pt) $ cell2Picture size c) cells
    where
      cells = map (\(coord,cell) -> (coord2Point d coord, cell)) $ mainCells b
      size  = 9

main :: IO ()
main = do
  seed <- randomIO
  let start = randomBoard seed
      d = InWindow "Life" (800,800) (10,10)
--  display d black $ board2Picture d start
  simulate d black 10 start (board2Picture d) (\_ _ b -> updateBoard b)

coord2Point :: Display -> Coordinate -> Point
coord2Point d (x,y) = (fromIntegral $ 10 * x - xOffset, fromIntegral $ yOffset - y * 10)
    where
      (xSize, ySize) = displaySize d
      (xOffset, yOffset) = (xSize `div` 2, ySize `div` 2)

square ::Float -> Picture
square size = Polygon [(0,0), (0, 0+size), (0+size, 0+size), (0+size, 0)]

