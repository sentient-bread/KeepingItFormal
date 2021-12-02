import AllTypes
import Translate
import Parse
import Model
import Evaluator

main = do
      line <- getLine
      let p = (simplify . translate . scanparse) line
      putStrLn $ show p
      putStrLn $ "The truth value is " ++ (show $ eval p model g (W 1, T 2))
