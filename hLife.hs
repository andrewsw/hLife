import Life.Console (loop, showBoard)
import Life (randomBoard)

import System.Random
import System.IO

main :: IO ()
main = do
       hSetBuffering stdin NoBuffering -- we want single-key commands...
       seed <- randomIO
       let b = randomBoard seed
       showBoard b 0
       loop b 0
