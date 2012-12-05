import Life (randomBoard)
import Life.Console as C (play)
import Life.Gloss as G (play)

import System.Random
import System.Environment
import System.Console.GetOpt

data Options = Options { useGloss   :: Bool
                       , width      :: Int
                       , height     :: Int
                       , inputFile  :: Maybe FilePath
                       , squareSize :: Int
                       }

defaultOptions :: Options
defaultOptions =  Options { useGloss   = False
                          , width      = 50
                          , height     = 50
                          , inputFile  = Nothing
                          , squareSize = 10
                          }

main :: IO ()
main = do
       args <- getArgs
       let (parsedOpts, _, msgs) = getOpt Permute options args
           opts = foldl (\o f -> f o) defaultOptions parsedOpts
       case msgs of
         [] -> do
           seed <- randomIO
           let b = randomBoard seed (width opts) (height opts)
           if useGloss opts
             then G.play b $ squareSize opts
             else C.play b
         _ -> error $ concat msgs ++ usageInfo "Usage: hLife [OPTION...]" options

options :: [ OptDescr (Options -> Options) ]
options =  [ Option ['s'] ["size"]  (ReqArg readSize "SIZE") $
             "board size as <width>x<height>. Default size "
             ++ show (width defaultOptions) ++ "x" ++ show (height defaultOptions)
           , Option ['g'] ["gloss"] (NoArg setGloss)         "use the Gloss graphical interface"
           , Option ['f'] ["file"]  (ReqArg setFile "FILE")  "read the supplied input file (will center in SIZE)"
           , Option ['q'] ["square" ] (ReqArg readSquare "SQUARE-SIZE") $
             "the size, in pixels of a square (assumes -g). Default "
             ++ show (squareSize defaultOptions)
           ]

setGloss :: Options -> Options
setGloss opts = opts { useGloss = True }

readSquare :: String -> Options -> Options
readSquare sqSize opts = opts { squareSize = sqSize', useGloss = True }
    where
      sqSize' = read sqSize

readSize :: String -> Options -> Options
readSize size opts = opts { width = w, height = h }
  where w = read $ takeWhile (/='x') size
        h = read . tail $ dropWhile (/= 'x') size

setFile :: FilePath -> Options -> Options
setFile file opts = opts { inputFile = Just file }
