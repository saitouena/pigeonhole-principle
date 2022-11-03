import Data.List

type AP = String -- atomic proposition
data Lit = Pos AP | Neg AP
  deriving (Eq, Show)
type Clause = [Lit]
type CNF = [Clause]
data Result = Refuted [Clause] | Exhausted [Clause]
  deriving Show

sameClause :: Clause -> Clause -> Bool
sameClause c1 c2 = null (c1 \\ c2) && null (c2 \\ c1)

nubClauses :: [Clause] -> [Clause]
nubClauses cs = nubBy sameClause cs

emptyClause :: Clause
emptyClause = []

complement :: Lit -> Lit
complement (Pos p) = Neg p
complement (Neg p) = Pos p

resolveLit :: Lit -> Clause -> [Clause]
resolveLit _l1 [] = []
resolveLit l1 (l2 : c2)
  | complement l1 == l2 = [c2]
  | otherwise = do c <- resolveLit l1 c2
                   return (l2 : c)

-- >>> resolveLit (Pos "a1") [Neg "a1", Neg "b2"]
-- [[Neg "b2"]]
--

resolve :: Clause -> Clause -> [Clause]
resolve [] _c2 = []
resolve (l : c1) c2 = 
  [ nub (c1 ++ c) | c <- resolveLit l c2] ++ [ nub (l : c) | c <- resolve c1 c2 ]

-- >>> resolve [Pos "a1"] [Neg "a1", Neg "b1"]
-- [[Neg "b2"]]
-- >>> resolve [Pos ]

resolveClauses :: [Clause] -> [Clause] -> [Clause]
resolveClauses cs1 cs2 = nub $ do
  c1 <- cs1
  c2 <- cs2
  resolve c1 c2

php2 :: CNF
php2 = [[Pos "a1"], [Pos "b1"], [Neg "a1", Neg "b1"]] 

php3 :: CNF
php3 = [[a1, a2], [b1, b2], [c1, c2], [a1', b1'], [b1', c1'], [c1', a1'],  [a2', b2'], [b2', c2'], [c2', a2']]
  where
    a1 = Pos "a1"
    a2 = Pos "a2"
    b1 = Pos "b1"
    b2 = Pos "b2"
    c1 = Pos "c1"
    c2 = Pos "c2"
    a1' = complement a1
    a2' = complement a2
    b1' = complement b1
    b2' = complement b2
    c1' = complement c1
    c2' = complement c2

-- >>> resolveClauses php2 php2
-- [[Neg "b1"],[Neg "a1"]] 

refute' :: [Clause] -> [Clause] -> Result
refute' old new
  | null next_new = Exhausted (old ++ new)
  | elem emptyClause next_new = Refuted (old ++ new ++ next_new)
  | otherwise = refute' (old ++ new) next_new
  where next_new = nubClauses ((resolveClauses new old ++ resolveClauses new new) \\ (old ++ new))

refute :: CNF -> Result
refute cnf
  | elem emptyClause cnf = Refuted cnf
  | otherwise = refute' [] cnf

-- >>> refute php2
-- Refuted [[Pos "a1"],[Pos "b1"],[Neg "a1",Neg "b1"],[Neg "b1"],[Neg "a1"],[]]
--
-- >>> refute php3
-- Refuted [[Pos "a1",Pos "a2"],[Pos "b1",Pos "b2"],[Pos "c1",Pos "c2"],[Neg "a1",Neg "b1"],[Neg "b1",Neg "c1"],[Neg "c1",Neg "a1"],[Neg "a2",Neg "b2"],[Neg "b2",Neg "c2"],[Neg "c2",Neg "a2"],[Pos "a2",Neg "b1"],[Pos "a2",Neg "c1"],[Pos "a1",Neg "b2"],[Pos "a1",Neg "c2"],[Pos "b2",Neg "a1"],[Pos "b2",Neg "c1"],[Pos "b1",Neg "a2"],[Pos "b1",Neg "c2"],[Pos "c2",Neg "b1"],[Pos "c2",Neg "a1"],[Pos "c1",Neg "b2"],[Pos "c1",Neg "a2"],[Pos "a2",Pos "b2"],[Neg "b1",Neg "b2"],[Neg "b1",Neg "c2"],[Pos "a2",Pos "c2"],[Neg "c1",Neg "b2"],[Neg "c1",Neg "c2"],[Pos "a1",Pos "b1"],[Pos "a1",Pos "c1"],[Neg "a1",Neg "a2"],[Neg "a1",Neg "c2"],[Pos "b2",Pos "c2"],[Neg "c1",Neg "a2"],[Pos "b1",Pos "c1"],[Neg "b1",Neg "a2"],[Neg "a1",Neg "b2"],[Neg "b1",Pos "b1"],[Pos "a2",Neg "a2"],[Pos "a2",Neg "c2"],[Neg "b1",Pos "c1"],[Neg "c1",Pos "b1"],[Pos "a2",Neg "b2"],[Neg "c1",Pos "c1"],[Neg "b2",Pos "b2"],[Pos "a1",Neg "a1"],[Pos "a1",Neg "c1"],[Neg "b2",Pos "c2"],[Neg "c2",Pos "b2"],[Pos "a1",Neg "b1"],[Neg "c2",Pos "c2"],[Neg "a1",Pos "c1"],[Pos "b2",Neg "a2"],[Neg "a2",Pos "c2"],[Pos "b1",Neg "a1"],[Pos "b2",Neg "b2"],[Pos "b2",Neg "c2"],[Pos "a2",Pos "a1"],[Pos "b2",Pos "b1"],[Pos "a2",Pos "c1"],[Pos "b2",Pos "c1"],[Neg "b1",Neg "a1"],[Neg "b2",Neg "a2"],[Neg "c2"],[Neg "b1"],[Pos "c2",Neg "b2"],[Pos "c2",Neg "c2"],[Pos "c2",Pos "b1"],[Pos "a2",Pos "b1"],[Pos "c2",Pos "c1"],[Neg "c1"],[Neg "b2"],[Neg "c1",Neg "b1"],[Neg "c2",Neg "b2"],[Pos "b1",Neg "b1"],[Pos "b1",Neg "c1"],[Pos "a1",Pos "c2"],[Pos "c1",Neg "b1"],[Pos "c1",Neg "c1"],[Pos "a1",Pos "b2"],[Neg "a2",Pos "a2"],[Neg "a1",Pos "a1"],[Neg "a1",Neg "c1"],[Neg "a2",Neg "c2"],[Neg "c2",Pos "a2"],[Neg "a1"],[Pos "c2",Neg "a2"],[Neg "c1",Pos "a1"],[Neg "a2"],[Pos "c1",Neg "a1"],[Neg "b1",Pos "a1"],[Neg "a2",Pos "b2"],[Neg "b2",Pos "a2"],[Neg "a1",Pos "b1"],[Neg "b1",Pos "a2"],[Neg "b1",Pos "c2"],[Neg "a2",Pos "b1"],[Neg "a2",Pos "c1"],[Neg "c2",Pos "b1"],[Pos "a2",Neg "a1"],[Neg "c2",Pos "c1"],[Neg "b1",Pos "b2"],[Neg "c1",Pos "a2"],[Neg "c1",Pos "c2"],[Pos "b1",Neg "b2"],[Neg "b2",Pos "c1"],[Neg "c1",Pos "b2"],[Neg "b2",Pos "a1"],[Neg "a1",Pos "b2"],[Neg "a1",Pos "c2"],[Pos "a1",Neg "a2"],[Neg "c2",Pos "a1"],[Pos "b2",Pos "a2"],[Pos "a2"],[Pos "b2"],[Neg "b2",Neg "b1"],[Neg "b2",Neg "c1"],[Neg "b2",Neg "a1"],[Neg "c2",Neg "b1"],[Neg "c2",Neg "c1"],[Neg "c2",Neg "a1"],[Pos "c2",Pos "a2"],[Pos "c2",Pos "b2"],[Pos "c2"],[Pos "b1",Pos "a1"],[Pos "a1"],[Pos "b1"],[Pos "c1",Pos "a1"],[Pos "c1"],[Pos "c1",Pos "b1"],[Neg "a2",Neg "a1"],[Neg "a2",Neg "c1"],[Neg "a2",Neg "b1"],[Pos "b2",Neg "b1"],[Pos "b1",Pos "a2"],[Pos "b1",Pos "c2"],[Pos "b2",Pos "a1"],[Pos "c1",Neg "c2"],[Pos "c1",Pos "a2"],[Pos "c1",Pos "b2"],[Pos "c2",Neg "c1"],[Neg "b2",Pos "b1"],[Pos "c2",Pos "a1"],[Neg "a2",Pos "a1"],[Neg "a1",Pos "a2"],[Neg "b2",Pos "b2"],[Pos "b2",Neg "c2"],[Pos "b2",Pos "b1"],[Neg "b2",Neg "a2"],[Neg "b2",Pos "c2"],[Neg "b2"],[Neg "b2",Neg "c2"],[Pos "b2",Neg "a2"],[Neg "b2",Pos "a2"],[Neg "b2",Pos "c1"],[Pos "b2",Neg "c1"],[Neg "b2",Pos "a1"],[Pos "b2",Neg "a1"],[Pos "b2",Pos "a2"],[Pos "b2"],[Neg "b2",Neg "b1"],[Neg "b2",Neg "c1"],[Neg "b2",Neg "a1"],[Pos "b2",Pos "c2"],[Neg "c2",Neg "a2"],[Neg "c2",Pos "c2"],[Neg "c2"],[Neg "c2",Pos "a2"],[Neg "c2",Pos "b1"],[Neg "c2",Pos "a1"],[Neg "c2",Neg "b1"],[Neg "c2",Neg "c1"],[Neg "c2",Neg "a1"],[Pos "a2",Neg "b1"],[Pos "a1",Pos "a2"],[Pos "a2",Neg "c1"],[Pos "a2"],[Pos "a1"],[Pos "a1",Pos "b1"],[Pos "a1",Pos "c1"],[Pos "a2",Pos "c2"],[Pos "a1",Neg "a1"],[Pos "a2",Neg "a2"],[Pos "a1",Neg "c1"],[Pos "a1",Neg "b1"],[Pos "b1",Neg "a2"],[Pos "b1"],[Pos "b1",Pos "c1"],[Pos "b1",Neg "b1"],[Pos "b1",Neg "c1"],[Pos "b1",Neg "a1"],[Pos "c1",Pos "c2"],[Pos "c1"],[Pos "c1",Neg "a1"],[Pos "c1",Neg "c1"],[Pos "c1",Neg "b1"],[Pos "c1",Neg "a2"],[Neg "a1",Pos "c2"],[Neg "a1",Neg "b1"],[Neg "a1",Neg "c1"],[Neg "b1",Pos "c2"],[Neg "b1",Neg "c1"],[Neg "b1"],[Neg "a1"],[Neg "a1",Neg "a2"],[Neg "b1",Neg "a2"],[Neg "a2"],[Neg "a2",Neg "c1"],[Neg "a2",Pos "c2"],[Neg "c1"],[],[Pos "c2"]]
--
-- >>> refute [[Pos "a"], [Pos "b"]]
-- Exhausted [[Pos "a"],[Pos "b"]]
--
