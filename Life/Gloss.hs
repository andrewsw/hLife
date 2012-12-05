module Life.Gloss (play) where

import Life
import Graphics.Gloss hiding (play)

displaySize :: Display -> (Int, Int)
displaySize (InWindow _ (x, y) _) = (x, y)
displaySize (FullScreen (x, y))   = (x, y)

cell2Picture :: Float -> Cell -> Picture
cell2Picture size c = Pictures [ cellColor c $ square size, Color black $ outline size ]
    where
      cellColor Alive = Color $ greyN 0.3
      cellColor Dead  = Color orange

board2Picture :: Display -> Int -> Board -> Picture
board2Picture d size b = Pictures $ map (\(pt, c) -> Translate (fst pt) (snd pt) $ picture c) cells
    where
      cells = map (\(coord,cell) -> (coord2Point d size coord, cell)) $ allCells b
      picture = cell2Picture (fromIntegral size)

play :: Board -> Int -> IO ()
play = flip loop 0

loop :: Board -> Int -> Int -> IO ()
loop start _ size = do
  let (_, (w, h)) = boardSize start
      d = InWindow "Life" (w * size,h * size) (10,10)
  simulate d black 8 start (board2Picture d size) (\_ _ b -> updateBoard b)

coord2Point :: Display -> Int -> Coordinate -> Point
coord2Point d sqSize (x,y) = (fromIntegral $ sqSize * x - xOffset, fromIntegral $ yOffset - y * sqSize)
    where
      (xSize, ySize) = displaySize d
      (xOffset, yOffset) = (xSize `div` 2, ySize `div` 2)

square :: Float -> Picture
square = Polygon . squarePath

outline :: Float -> Picture
outline = Line . squarePath

squarePath :: Float -> Path
squarePath size = [(0,0), (0, 0+size), (0+size, 0+size), (0+size, 0)]