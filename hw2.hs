-- CS 381 HW 2
-- Glenn Upthagrove, Brian Ozarowicz, David Baugh

-- Exercise 1
type Prog = [Cmd]
data Cmd = LD Int | ADD | MULT | DUP | DEF String Prog | CALL String
		deriving Show

type Stack = [Int]
type D = Stack -> Maybe Stack

semCmd :: Cmd -> D
semCmd (LD i) xs = Just(i:xs)
semCmd (ADD) (x1:x2:xs) = Just(x1+x2:xs)
semCmd (ADD) (_) = Nothing
semCmd (MULT) (x1:x2:xs) = Just(x1*x2:xs)
semCmd (MULT) (_) = Nothing
semCmd (DUP) (x:xs) = Just (x:x:xs)
semCmd (DUP) (_) = Nothing

sem :: Prog -> D
sem [] ys = Just (ys)
sem (x:xs) ys = case semCmd x ys of
                  Nothing -> Nothing
                  Just s -> sem xs s

test1 = [LD 3, DUP, ADD, DUP, MULT]
test2 = [LD 3, ADD]
test3 = []
testa = [LD 3, DUP, ADD]
testb = []
testc = sem testa testb

-- Exercise 2a
-- Syntax extended in Excercise 1 data definition above

-- Exercise 2b
type Macros = [(String,Prog)]

type State = Maybe (Macros, Stack)

-- Exercise 2c


-- Exercise 3
data Cmd3 = Pen Mode | MoveTo Int Int | Seq Cmd3 Cmd3
		deriving Show

data Mode = Up | Down
		deriving Show

type State3 = (Mode, Int, Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd3 -> State3 -> (State3, Lines)
semS (Pen Up) (mode, x, y) = ((Up, x, y), [])
semS (Pen Down) (mode, x, y) = ((Down, x, y), [])
semS (MoveTo x1 y1) (Up, x2, y2) = ((Up, x2, y2), [])
semS (MoveTo x1 y1) (Down, x2, y2) = ((Down, x1, y1), [(x2, y2, x1, y1)])
-- Seq end state is the result of running cmd2 on the result of running cmd1 on the original state
semS (Seq cmd1 cmd2) state = (fst(semS cmd2 (fst(semS cmd1 state))), snd(semS cmd1 state)++snd(semS cmd2 (fst(semS cmd1 state))))

sem' :: Cmd3 -> Lines
sem' initial = snd (semS initial (Up, 0, 0))
