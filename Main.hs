import AllTypes
import Translate
import Parse

main = do
      line <- getLine
      let p = (simplify . translate . scanparse) line
      putStrLn $ show p
