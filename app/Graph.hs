{-# LANGUAGE DeriveGeneric #-}

module Graph (topologicalSort, taskName, detectCycles) where

import Types (Task(..), TaskNode(..), TaskGraph(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (foldM)

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

topologicalSort :: [Task] -> Either String [TaskNode]
topologicalSort ts =
  let TaskGraph nodeMap = buildTaskGraph ts
      nodes = map snd nodeMap
  in dfsAll nodes [] []

dfsAll :: [TaskNode] -> [TaskNode] -> [String] -> Either String [TaskNode]
dfsAll [] sorted _ = Right sorted
dfsAll (n:ns) sorted visited =
  if taskName n elem visited
    then dfsAll ns sorted visited
    else do
      (visited', sorted') <- dfs n visited sorted []
      dfsAll ns sorted' visited'

dfs :: TaskNode -> [String] -> [TaskNode] -> [String] -> Either String ([String], [TaskNode])
dfs node visited sorted recStack
  | current elem recStack = Left $ "Ciclo detectado en la tarea: " ++ current
  | current elem visited  = Right (visited, sorted)
  | otherwise = do
      (visited', sorted') <- foldM
        (\(vAcc, sAcc) dep -> dfs dep vAcc sAcc (current : recStack))
        (visited, sorted)
        (dependencies node)
      Right (current : visited', node : sorted')
  where
    current = taskName node
