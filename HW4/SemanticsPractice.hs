module SemanticsPractice where

--
-- * Example 1
--

data Stmt = Inc Int
          | Reset
  deriving (Eq,Show)

type CounterProg = [Stmt]

data AAA = Int | Bool
-- Semantic domain for Stmt: Int -> Int

-- | Semantics of programs.
--
--   >>> cprog [Inc 3, Inc 5, Reset, Inc 4, Inc 6]
--   10
--   >>> cprog [Inc 3, Inc 5, Inc 4, Inc 6]
--   18

stmt :: Stmt -> Int -> Int
stmt (Inc i) c = c+i
stmt Reset   _ = 0

stmts :: [Stmt] -> Int -> Int
stmts []     c = c
stmts (s:ss) c = stmts ss (stmt s c)

cprog :: CounterProg -> Int
cprog p = stmts p 0

-- | Semantics of programs.
--
--   >>> ccprog [Inc 3, Inc 5, Reset, Inc 4, Inc 6]
--   10
--   >>> ccprog [Inc 3, Inc 5, Inc 4, Inc 6]
--   18

ccprog :: CounterProg -> Int
ccprog [] = 0
ccprog s = stmt (last s) (ccprog (init s))

--
-- * Example 2
--

data Cmd = Gas
         | Brake
         | Turn
  deriving (Eq,Show)

type Prog = [Cmd]

type Pos   = Int
type Speed = Int

data Dir = Forward | Backward
  deriving (Eq,Show)

data Result = OK Pos Speed Dir
            | Crash Pos
  deriving (Eq,Show)

-- Semantic domain for Cmd:  Pos -> Speed -> Dir -> Result

cmd :: Cmd -> Pos -> Speed -> Dir -> Result
cmd Gas   p s d = OK (move d p s) (s+1) d
cmd Brake p 0 d = OK p 0 d
cmd Brake p s d = OK (move d p s) (s-1) d
cmd Turn  p 0 d = OK p 0 (turn d)
cmd Turn  p _ _ = Crash p

turn :: Dir -> Dir
turn Forward  = Backward
turn Backward = Forward

move :: Dir -> Pos -> Speed -> Pos
move Forward  p s = p+s
move Backward p s = p-s

-- | Semantics of programs.
--
--   >>> prog [Gas, Gas, Gas, Brake] 0 0 Forward
--   OK 6 2 Forward
--
--   >>> prog [Gas, Brake, Gas, Brake] 0 0 Backward
--   OK (-2) 0 Backward
--
--   >>> prog [Gas, Gas, Brake, Turn, Gas, Gas] 0 0 Forward
--   Crash 3
--
--   >>> prog [Gas, Gas, Brake, Turn] 0 0 Forward
--   Crash 3
--
--   >>> prog [Gas, Gas, Brake, Brake, Turn, Gas, Gas] 0 0 Forward
--   OK 3 2 Backward
--
prog :: Prog -> Pos -> Speed -> Dir -> Result
prog []     p s d = OK p s d
prog (c:cs) p s d = case cmd c p s d of
                      OK p' s' d' -> prog cs p' s' d'
                      crash       -> crash
