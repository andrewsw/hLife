import Life (randomBoard)
import Life.Console as C (play, showBoard)
import Life.Gloss as G (play)

import System.Random
import System.Environment

defaultSize = 50 :: Int

main :: IO ()
main = do
       args <- getArgs
       let (width, height) = case args of
             a@[_, _]       | [w', h'] <- map read a -> (w', h')
             a@["-g", _, _] | [w', h'] <- map read (tail a) -> (w', h')
             _ -> (defaultSize, defaultSize)
       seed <- randomIO
       let b = randomBoard seed width height
       case args of
         ("-g" : _) -> G.play b
         _          -> C.play b
