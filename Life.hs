module Life where

import Data.Array as A
import Data.List.Split (chunk)
import System.Random

data Cell = Alive | Dead

instance Random Cell where
  randomR (a,b) g = case (randomR (cell2Bool a, cell2Bool b) g) of
                      (x, g') -> (bool2Cell x, g')
                     where
                       cell2Bool :: Cell -> Bool
                       cell2Bool Alive = True
                       cell2Bool Dead = False

                       bool2Cell :: Bool -> Cell
                       bool2Cell True = Alive
                       bool2Cell False = Dead

  random g = randomR (Alive, Dead) g

instance Show Cell where
  show Alive = "O"
  show Dead  = "."

type Coordinate = (Int, Int)

type Board = Array Coordinate Cell

ppBoard :: Board -> String
ppBoard board = unlines $ fmap concat rows
  where rows = do
        let ((xmin, ymin), (xmax, ymax)) = bounds board
        y <- [ ymin .. ymax ]
        let row y' = do
              x <- [ xmin .. xmax ]
              return $ show $ board ! (x,y')
        return $ row y


mkBoard :: [ [ Cell ] ] -> Board
mkBoard rows = array ((0,0), (xlen - 1, ylen - 1)) $  zip coords $ concat rows
    where
      (xlen, ylen) = (length $ head rows, length rows)
      coords = [ (x,y) | y <- [0..ylen - 1], x <- [0..xlen - 1] ]

updateBoard :: Board -> Board
updateBoard b = b // newCells
    where
      newCells = map (\c -> (c, updateCell b c)) $ indices b

allCells :: Board -> [(Coordinate, Cell)]
allCells b = assocs b

updateCell :: Board -> Coordinate -> Cell
updateCell  board c@(x,y) | neighborCount > 3  = Dead
                          | neighborCount < 2  = Dead
                          | neighborCount == 3 = Alive
                          | neighborCount < 3  = cell
  where cell = board ! c
        addNeighbor = \c' i -> case board ! c' of
          Alive -> i + 1
          Dead -> i
        neighborCount = foldr addNeighbor 0 $ cellNeighbors bs c
        bs = bounds board

cellNeighbors :: (Coordinate, Coordinate) -> Coordinate -> [ Coordinate ]
cellNeighbors bs@((xmin, ymin), (xmax, ymax)) (x,y) = filter boundsCheck neighbors
    where
      boundsCheck (x', y') = xmin <= x'
                              && x' <= xmax
                              && ymin <= y'
                              && y' <= ymax
      neighbors = [ (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
                  , (x - 1, y    ),             (x + 1, y    )
                  , (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
                  ]

randomBoard :: Int -> Board
randomBoard seed =
  let cells = take (50*50) $ randoms$ mkStdGen seed :: [ Cell ]
  in  mkBoard $ chunk 50 cells

main :: IO ()
main = do
       seed <- randomIO
       let b = randomBoard seed
       loop b 0

loop :: Board -> Int -> IO ()
loop b g = do
  putStrLn $ ppBoard b
  putStrLn $ "Generation number:" ++ show g
  input <- getLine
  case input of
    "q" -> return ()
    "d" -> do
      putStrLn $ ppBoard b
      loop b g
    _   -> do
      let b' = updateBoard b
      loop b' (g + 1)


