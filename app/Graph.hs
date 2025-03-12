{-# LANGUAGE DeriveGeneric #-}

module Graph(buildTaskGraph, topologicalSort, taskName) where

import Types (Workflow(..), Task(..), ExecutionState(..), TaskNode(..), TaskOutput(..), TaskGraph(..), RetryPolicy(..), FailStrategy(..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)

buildTaskGraph :: [Task] -> TaskGraph
buildTaskGraph tasks =
    let adjacencyList = [(name t, depends_on t) | t <- tasks]
        taskNodes = [(name t, TaskNode t []) | t <- tasks]
        populatedNodes = map (\(n, node) -> (n, populateDependencies node adjacencyList taskNodes)) taskNodes
    in TaskGraph populatedNodes

populateDependencies :: TaskNode -> [(String, [String])] -> [(String, TaskNode)] -> TaskNode
populateDependencies node adjacency taskNodes =
    let taskMap = M.fromList taskNodes
        depNames = fromMaybe [] (lookup (name . task $ node) adjacency)
        depNodes = mapMaybe (`M.lookup` taskMap) depNames
    in node { dependencies = depNodes }

topologicalSort :: TaskGraph -> [TaskNode]
topologicalSort (TaskGraph taskMap) = dfsAll (map snd taskMap) []

dfsAll :: [TaskNode] -> [TaskNode] -> [TaskNode]
dfsAll [] visited = visited
dfsAll (node:rest) visited
    | taskName node `elem` map taskName visited = dfsAll rest visited
    | otherwise = dfsAll rest (dfs (dependencies node) visited ++ [node])

dfs :: [TaskNode] -> [TaskNode] -> [TaskNode]
dfs [] visited = visited
dfs (x:xs) visited
    | taskName x `elem` map taskName visited = dfs xs visited
    | otherwise = dfs xs (x : visited)

taskName :: TaskNode -> String
taskName = name . task
