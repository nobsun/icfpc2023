
import System.Environment (getArgs)

import Solver (Name, SolverF)
import qualified Solver.Arraying as Arraying
import qualified Solver.NumericOptimization as NumericOptimization
import qualified Solver.Genetic as Genetic
import qualified Solver.FrontHeat as FrontHeat
import Solutions

solvers :: [(Name, SolverF)]
solvers =
  [ ("arraying", Arraying.getCandidates)
  , ("numeric-optimization", NumericOptimization.getCandidates)
  , ("genetic", Genetic.getCandidates)
  , ("front-heat", FrontHeat.getCandidates FrontHeat.Lattice)
  , ("front-heat-triangle", FrontHeat.getCandidates FrontHeat.Triangle)
  ]

main :: IO ()
main = do
  let help = putStrLn $ "supported solvers: " ++ unwords (map fst solvers)

  args <- getArgs
  (name, pnum) <- case args of
    name : pnum : _ -> (,) name <$> readIO pnum
    _               -> help *> fail "NAME and PROBLEM_ID required."

  let run solver = saveAnswer (name, solver) pnum
  maybe help run $ lookup name solvers
