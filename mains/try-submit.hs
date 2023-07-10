
import System.Environment
import Submit

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : _  ->  trySubmitHeuristic path
    []     ->  fail "FILENAME required."
