-- CS 381 HW 1
-- Glenn Upthagrove, Brian Ozarowicz, David Baugh

-- 1A
data Cmd = Pen Mode
        | Moveto Pos Pos
        | Def String [String] Cmd
        | Call String [Int]
        | Seq Cmd Cmd
        deriving Show

data Mode = Up | Down
data Pos = PosA Int | PosB String

instance Show Pos where
        show (PosA i) = show i
        show (PosB j) = show j
instance Show Mode where
        show (Up) = show "Up"
        show (Down) = show "Down"

--test1 = Pen Down
--test2 = Seq (Pen Up) (Pen Down)
--test3 = Moveto (PosA 1) (PosA 2)
--test4 = Moveto (PosB "1") (PosB "2")
--main = print test1

-- 1B
-- def vector (x1, y1, x2, y2) pen up; moveto (x1, y1); pen down; moveto (x2, y2)
vector :: Cmd
vector = Def "vector" ["x1", "y1", "x2", "y2"]
		(Seq (Pen Up)
		(Seq (Moveto (PosB "x1") (PosB "y1"))
		(Seq (Pen Down)
		(Moveto (PosB "x2") (PosB "y2")))))

--test1 = vector
--main = print test1

-- 1C
steps :: Int -> Cmd
steps 1 = Seq (Pen Up) (Seq (Moveto (PosA 0) (PosA 0)) (Seq (Pen Down) (Seq (Moveto (PosA 0) (PosA 1)) ((Moveto (PosA 1) (PosA 1))))))
steps n = Seq (steps (n-1)) (Seq (Moveto (PosA (n-1)) (PosA n)) (Moveto (PosA n) (PosA n)))

--test1 = steps 1
--test2 = steps 3
--main = print test2