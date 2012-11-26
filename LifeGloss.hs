module LifeGloss where

import Life (randomBoard, Cell(..), Board, allCells, updateBoard, Coordinate)
import System.Random
import Graphics.Gloss

displaySize :: Display -> (Int, Int)
displaySize (InWindow _ (x, y) _) = (x, y)
displaySize (FullScreen (x, y))   = (x, y)


cell2Picture :: Float -> Cell -> Picture
cell2Picture size c = Pictures [ cellColor c $ square size, Color black $ outline size ]
    where
      cellColor Alive = Color $ greyN 0.3
      cellColor Dead  = Color orange

board2Picture :: Display -> Board -> Picture
board2Picture d b = Pictures $ map (\(pt, c) -> Translate (fst pt) (snd pt) $ cell2Picture size c) cells
    where
      cells = map (\(coord,cell) -> (coord2Point d coord, cell)) $ allCells b
      size  = 10

main :: IO ()
main = do
  seed <- randomIO
  let start = randomBoard seed
      d = InWindow "Life" (800,600) (10,10)
  simulate d black 8 start (board2Picture d) (\_ _ b -> updateBoard b)

coord2Point :: Display -> Coordinate -> Point
coord2Point d (x,y) = (fromIntegral $ 10 * x - xOffset, fromIntegral $ yOffset - y * 10)
    where
      (xSize, ySize) = displaySize d
      (xOffset, yOffset) = (xSize `div` 2, ySize `div` 2)

square :: Float -> Picture
square = Polygon . squarePath

outline :: Float -> Picture
outline = Line . squarePath

squarePath :: Float -> Path
squarePath size = [(0,0), (0, 0+size), (0+size, 0+size), (0+size, 0)]