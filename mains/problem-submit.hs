
import System.Environment
import Submit

main :: IO ()
main = do
  args <- getArgs
  (pnum, fs) <- case args of
    pnum : fs  ->  (,) <$> readIO pnum <*> pure fs
    []     ->  fail "PROBLEM_ID and FILENAMES required."

  tryToSubmit pnum fs
