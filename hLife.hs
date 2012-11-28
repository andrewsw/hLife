import Life (randomBoard)
import Life.Console as C (loop, showBoard)
import Life.Gloss as G (loop)

import System.Random
import System.IO
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
         ("-g" : _) -> G.loop b 0
         _          -> do
           hSetBuffering stdin NoBuffering -- we want single-key commands...
           showBoard b 0
           C.loop b 0
