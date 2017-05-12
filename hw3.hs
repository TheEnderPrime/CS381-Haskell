-- CS 381 HW 3
-- Glenn Upthagrove, Brian Ozarowicz, David Baugh

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
