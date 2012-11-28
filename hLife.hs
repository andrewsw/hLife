import Life.Console (loop, showBoard)
import Life (randomBoard)

import System.Random
import System.IO
import System.Environment

defaultSize = 50 :: Int

main :: IO ()
main = do
       args <- getArgs
       let (width, height) = case args of
             a@[_,_] |  [w', h'] <- map read a -> (w', h')
             _ -> (defaultSize, defaultSize)
       hSetBuffering stdin NoBuffering -- we want single-key commands...
       seed <- randomIO
       let b = randomBoard seed width height
       showBoard b 0
       loop b 0
