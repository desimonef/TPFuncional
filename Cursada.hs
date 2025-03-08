data E = Cte Int | Div E E

eval :: E -> Int
eval (Cte n) = n
eval (Div e1 e2) = eval e1 `div` eval e2

type Output a = (a, Screen)
type Screen = String

evalP :: E -> Output Int
evalP (Cte n) = (n, "")
evalP (Div e1 e2) = let (v1,p1) = evalP e1
    in let (v2,p2) = evalP e2
        in let p3 = printf (formatDiv v1 v2 (v1 `div` v2))
            in (v1 `div` v2, p1++p2++p3)

type Variable = String
type Mem = (Variable, Int)
type StateT a = Mem -> (a, Mem)

inc :: Variable -> Mem -> Mem
inc "nDiv" ("nDiv", d) = ("nDiv", d+1)
