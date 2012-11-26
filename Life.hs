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
        y <- [ ymin + 1 .. ymax - 1 ]
        let row y' = do
              x <- [ xmin + 1 .. xmax - 1 ]
              return $ show $ board ! (x,y')
        return $ row y

dumpBoard board = unlines $ fmap concat rows
  where rows = do
        let ((xmin, ymin), (xmax, ymax)) = bounds board
        y <- [ ymin .. ymax ]
        let row y' = do
              x <- [ xmin .. xmax ]
              return $ show $ board ! (x,y')
        return $ row y


mkBoard :: [ [ Cell ] ] -> Board
mkBoard rows = array ((0,0), (xCount + 1, yCount + 1)) $  zip coords $ concat newRows
    where
      (yCount, xCount) = (length rows, length $ head rows)
      deadRow = replicate (xCount + 2) Dead
      newRows = deadRow : (map wrapRow rows) ++ [ deadRow ]
      coords = [ (x,y) | y <- [0..yCount + 1], x <- [0..xCount + 1] ]

wrapRow ::[ Cell ] -> [ Cell ]
wrapRow r = Dead : r ++ [ Dead ]


updateBoard :: Board -> Board
updateBoard b = b // newCells
    where
      newCells = mainCells b

mainCells :: Board -> [(Coordinate, Cell)]
mainCells b = map (\c -> (c, updateCell b c)) [ (x,y) | x <- [1..xCount - 1], y <- [1..yCount - 1] ]
    where
      (_, (xCount, yCount)) = bounds b

updateCell :: Board -> Coordinate -> Cell
updateCell  board c@(x,y) | neighbors > 3 = Dead
                          | neighbors < 2 = Dead
                          | neighbors == 3 = Alive
                          | neighbors < 3 = cell
  where cell = board ! c
        addNeighbor = \c' i -> case board ! c' of
          Alive -> i + 1
          Dead -> i
        neighbors = foldr addNeighbor 0 $ cellNeighbors c

cellNeighbors :: Coordinate -> [ Coordinate ]
cellNeighbors (x,y) =[ (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
                     , (x - 1, y    ),             (x + 1, y    )
                     , (x - 1, y - 1), (x, y - 1), (x + 1, y - 1) ]

randomBoard :: Int -> Board
randomBoard seed =
  let cells = take (75*75) $ randoms$ mkStdGen seed :: [ Cell ]
  in  mkBoard $ chunk 75 cells

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
      putStrLn $ dumpBoard b
      loop b g
    _   -> do
      let b' = updateBoard b
      loop b' (g + 1)


