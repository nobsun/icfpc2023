
import Solutions

main :: IO ()
main = do
  in' <- getContents
  putStr $ unlines $ problemSubmitCommands $ lines in'
