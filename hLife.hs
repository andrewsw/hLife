import Life (randomBoard)
import Life.Console as C (play)
import Life.Gloss as G (play)

import System.Random
import System.Environment
import System.Console.GetOpt

data Options = Options { useGloss  :: Bool
                       , width     :: Int
                       , height    :: Int
                       , inputFile :: Maybe FilePath
                       }

defaultOptions :: Options
defaultOptions =  Options { useGloss  = False
                          , width     = 50
                          , height    = 50
                          , inputFile = Nothing
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
             then G.play b
             else C.play b
         _ -> error $ concat msgs ++ usageInfo "Usage: hLife [OPTION...]" options

options :: [ OptDescr (Options -> Options) ]
options =  [ Option ['s'] ["size"]  (ReqArg readSize "SIZE") "board size as <width>x<height>"
           , Option ['g'] ["gloss"] (NoArg setGloss)         "use the Gloss graphical interface"
           , Option ['f'] ["file"]  (ReqArg setFile "FILE")  "read the supplied input file (will center in SIZE)"
           ]

setGloss :: Options -> Options
setGloss opts = opts { useGloss = True }

readSize :: String -> Options -> Options
readSize size opts = opts { width = w, height = h }
  where w = read $ takeWhile (/='x') size
        h = read . tail $ dropWhile (/= 'x') size

setFile :: FilePath -> Options -> Options
setFile file opts = opts { inputFile = Just file }
