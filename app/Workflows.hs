
module Workflows (executeWorkflow, buildPlans) where

import Graph (topologicalSort)
import Types
  ( Workflow(..), Task(..), TaskNode(..)
  , TaskOutput(..), RetryPolicy(..)
  , FailStrategy(..), ExecutionPlan(..)
  )
import Control.Applicative ((<|>))

executeWorkflow :: Workflow -> Either String [ExecutionPlan]
executeWorkflow (Workflow _ ts) =
  let ordered = topologicalSort ts
      (plans, success) = buildPlans ordered []
  in if success
       then Right plans
       else Left "Faltan comandos o scripts en una o más tareas."

buildPlans :: [TaskNode] -> [(String, TaskOutput)] -> ([ExecutionPlan], Bool)
buildPlans [] _ = ([], True)
buildPlans (tn:rest) state =
  let t = task tn
      unresolved = input t
  in case command t <|> script t of
      Nothing -> ([], False)
      Just cmd ->
        let out = taskToTaskOutput t
            (retries, strategy) = extractRetryPolicy (retryPolicy t)
            plan = ExecutionPlan (name t) cmd unresolved out retries strategy
            newState = (name t, out) : state
            (restPlans, ok) = buildPlans rest newState
        in (plan : restPlans, ok)

extractRetryPolicy :: Maybe RetryPolicy -> (Int, FailStrategy)
extractRetryPolicy (Just (RetryPolicy n strat)) = (n, strat)
extractRetryPolicy Nothing = (0, FailWorkflow)

taskToTaskOutput :: Task -> TaskOutput
taskToTaskOutput t = maybe (OutputValue "") OutputFile (output t)
