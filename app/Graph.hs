
module Graph (topologicalSort, taskName) where

import Types (Task(..), TaskNode(..), TaskGraph(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)

buildTaskGraph :: [Task] -> TaskGraph
buildTaskGraph ts =
  let adjacency = [(name t, depends_on t) | t <- ts]
      nodes = [(name t, TaskNode t []) | t <- ts]
      complete = map (\(n, node) -> (n, populateDependencies node adjacency nodes)) nodes
  in TaskGraph complete

populateDependencies :: TaskNode -> [(String, [String])] -> [(String, TaskNode)] -> TaskNode
populateDependencies node adj allNodes =
  let depNames = fromMaybe [] (lookup (name . task $ node) adj)
      nodeMap = M.fromList allNodes
      deps = mapMaybe (`M.lookup` nodeMap) depNames
  in node { dependencies = deps }

taskName :: TaskNode -> String
taskName = name . task

topologicalSort :: [Task] -> [TaskNode]
topologicalSort ts =
  let TaskGraph nodeMap = buildTaskGraph ts
      nodes = map snd nodeMap
  in reverse (dfsAll nodes [])

dfsAll :: [TaskNode] -> [TaskNode] -> [TaskNode]
dfsAll [] sorted = sorted
dfsAll (n:ns) sorted
  | taskName n `elem` map taskName sorted = dfsAll ns sorted
  | otherwise = dfsAll ns (dfs n sorted)

dfs :: TaskNode -> [TaskNode] -> [TaskNode]
dfs node sorted
  | taskName node `elem` map taskName sorted = sorted
  | otherwise =
      let dfsSorted = foldr dfs sorted (dependencies node)
      in node : dfsSorted
