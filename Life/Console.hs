module Life.Console ( play
                    , showBoard
                    ) where

import Life

import System.IO
import Control.Monad (when)

play :: Board -> IO ()
play b = do
  hSetBuffering stdin NoBuffering -- we want single key commands
  showBoard b 0
  loop b 0

loop :: Board -> Int -> IO ()
loop b g = do
  choice <- prompt
  case choice of
    'h' -> showHelp >> loop b g
    'q' -> return ()
    'd' -> showBoard b g >> loop b g
    ' ' -> let b' = updateBoard b
               g' = g + 1
           in showBoard b' g' >> loop b' g'
    _   -> showHelp >> loop b g

prompt :: IO Char
prompt = do
  putStr "Enter command (h for help): "
  hFlush stdout
  input <- getChar
  when (input /= '\n') $ putStr "\n"
  return input

showBoard :: Board -> Int -> IO ()
showBoard b g = do
  putStrLn $ "Generation number:" ++ show g
  putStrLn $ ppBoard b

ppBoard :: Board -> String
ppBoard board = unlines $ fmap concat rows
  where rows = do
        let ((xmin, ymin), (xmax, ymax)) = boardSize board
        y <- [ ymin .. ymax ]
        let row y' = do
              x <- [ xmin .. xmax ]
              return . show $ boardCell board (x, y')
        return $ row y

showHelp :: IO ()
showHelp = mapM_ putStrLn [ "Options:"
                          , "h - this help"
                          , "d - re-display the current board"
                          , "q - quit"
                          , "<space> - next generation"
                          ]
