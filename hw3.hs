-- CS 381 HW 3
-- Glenn Upthagrove, Brian Ozarowicz, David Baugh

-- Exercise 1A
type Prog = [Cmd]
data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int deriving Show

type Rank = Int
type CmdRank = (Int, Int)

rankC :: Cmd -> CmdRank
rankC (LD i) = (0, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC DUP = (1, 2)
rankC INC = (1, 1)
rankC SWAP = (2, 2)
rankC (POP i) = (i, 0)

rankP :: Prog -> Maybe Rank
rankP xs = rank xs 0 

rank :: Prog -> Rank -> Maybe Rank
rank [] k = Just k
rank (x:xs) k = let (n, m) = rankC x in
			 if k < n then Nothing else rank xs ((k - n) + m)
       -- next stack elements = ((current - taken) + added) by the first operation

-- Exercise 1B
-- (This section set as comments so the file will compile, due to incomplete function definition
--   as allowed in the instructions, but our answer to the HW question is complete.)

-- Maybe types and their Nothing cases can be removed because static checking guarantees there are no errors.
{-
semStatTC :: Prog -> Maybe Stack
semStatTC xs = case rankP xs of
			Nothing -> Nothing
			Just k -> Just (sem [] xs)

sem :: Prog -> Stack -> Stack
sem [] ys = ys
sem (x:xs) ys = sem xs (semCmd x ys)

semCmd :: Cmd -> Stack -> Stack
semCmd (LD i) xs = i:xs
semCmd (ADD) (x1:x2:xs) = (x1+x2:xs)
semCmd (MULT) (x1:x2:xs) = (x1*x2:xs)
semCmd (DUP) (x:xs) = (x:x:xs)
semCmd (INC) (x:xs) = ((x+1):xs)
semCmd (SWAP) (x1:x2:xs) = (x2:x1:xs)
semCmd (POP i) xs = drop i xs
-}

-- Exercise 2A
data Shape = X
            | TD Shape Shape
            | LR Shape Shape
            deriving Show

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox (X) = (1, 1)
bbox (TD i j) =
	let (x1, y1) = bbox i
	    (x2, y2) = bbox j
	in (max x1 x2, y1+y2)
bbox (LR i j) =
	let (x1, y1) = bbox i
	    (x2, y2) = bbox j
	in (x1+x2, max y1 y2)


-- Exercise 2B
rect :: Shape -> Maybe BBox
rect (X) = Just (1, 1)
rect (TD i j) = case (rect i, rect j) of
				(Just (x1, y1), Just (x2, y2)) -> if (x1==x2) then (Just (x1, y1+y2)) else Nothing
				_ -> Nothing
rect (LR i j) = case (rect i, rect j) of
				(Just (x1, y1), Just (x2, y2)) -> if (y1==y2) then (Just (x1+x2, y1)) else Nothing
				_ -> Nothing

-- Exercise 3A
{-
f x y = if null x then [y] else x
g x y = if not (null x) then [] else [y]

-- 1)
f :: [a] -> a -> [a]
g :: [a] -> b -> [b]

-- 2)
f: Null takes a List so x must be [a]. The types of x and y must be the same to check the conditional.
g: Same as f, but now returns a different list and the type of y is not related to x anymore.

-- 3)
g is more general because for f the lists must be the same type.

-- 4)
f and g are different types because in f the list type is constrained, but not in g.
-}

-- Exercise 3B
h :: [b] -> [(a, b)] -> [b]
h param1 [(param2, param3)] = param3:param1

-- Exercise 3C
k :: (a -> b) -> ((a -> b) -> a) -> b
k param1 param2 = (param1 (param2 param1))

-- Exercise 3D
-- a->b is difficult because there is not an apparent way to take in type a and directly return type b.
