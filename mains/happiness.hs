
import System.Environment (getArgs)

import Solutions

main :: IO ()
main = do
  args <- getArgs
  (name, pnum) <- case args of
    name : pnum : _ -> (,) name <$> readIO pnum
    _               -> fail "NAME and PROBLEM_ID required."

  h <- calcHappiness name pnum
  putStrLn $ name ++ " " ++ show pnum ++ ": " ++ show h
