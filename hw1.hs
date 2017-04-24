--
-- CS 381 HW 1
-- Glenn Upthagrove, Brian Ozarowicz, David Baugh
--

-- 1A
data Cmd = Pen Mode
        | Moveto Pos Pos
        | Def Name Pars Cmd
        | Call Name Vals
        | Seq [Cmd]
        deriving Show

data Mode = Up | Down
data Pos = Posa Number | Posb Name
data Pars = ParsSingle Name | ParsMany [Name]
data Vals = ValsSingle Int | ValsMany [Int]

type Number = Int
type Name = String

instance Show Vals where
        show (ValsSingle i) = show i
instance Show Pars where
        show (ParsSingle i) = show i
        show (ParsMany j) = show j
instance Show Pos where
        show (Posa i) = show i
        show (Posb j) = show j
instance Show Mode where
        show (Up) = show "up"
        show (Down) = show "down"

-- testing
x = Up
test = ParsSingle "apple"
test2 = ParsMany ["apple", "berry"]
main = print test2

-- 1B
