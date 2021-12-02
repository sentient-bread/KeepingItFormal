module AllTypes where

-- English --
data Eng = CN String    | Tm String | Pron Int Char | IV String
        -- (common nouns) (terms)     (pronouns)      (intrans. v)
         | IAV String | TV String    | StV String
        -- (adverbs)    (trans. verbs) (sent-taking verbs)
         | ItV String           | SmA String
        -- (intrans-taking verbs) (sent-modifying adv)
         | Prep String  | Det String -- basic categories
        -- (prepositions) (determiners)
         | F2 Eng Eng                -- CNs (every, the, a)
        -- :: Det -> CN -> Tm
         | F3 Int Eng Eng            -- CN, T (relative clause)
        -- :: Int -> CN -> T -> CN
         | F4 Eng Eng                -- Term, IV (S-V)
        -- :: Tm -> IV -> T
         | F5 Eng Eng                -- TV, Term (V-O); Prep Term (PP)
        -- :: TV -> Tm -> IV
        -- :: Prep -> Tm -> IAV (prep-object)
         | F6 Eng Eng                -- SmA, T (necessarily, possibly T)
        -- :: SmA -> T -> T
         | F7 Eng Eng                -- IAV, IV (adverb-verb)
        -- :: IAV -> IV -> IV
         | F8 Eng Eng | F9 Eng Eng   -- Term, T, IV (and,or)
        -- :: a -> a -> a
         | F10 Int Eng Eng           -- Term, T (sub)
        -- :: Tm -> T -> T
         | F11 Eng Eng               -- StV, T (belives/thinks that T)
        -- :: StV -> T -> IV
         | F12 Eng Eng | F13 Eng Eng
         | F14 Eng Eng | F15 Eng Eng
         | F16 Eng Eng               -- Term, IV (tenses S-V)
        -- :: Tm -> IV -> T
         | F17 Eng Eng               -- ItV, IV (tries to IV)
        -- :: ItV -> IV -> IV
              deriving (Show, Eq)

transl :: Eng -> LExpr -- all expressions
transl _ = LCon ""
-------------

-- Intensional Logic --
data World = W Int
              deriving (Show, Eq)
data Time  = T Int
              deriving (Show, Eq, Ord)

type Index = (World, Time) -- indices
type S = Index

data Denot = Indv String           -- individual  (e)
           | TVal Bool             -- truth value (t)
           | Func (Denot -> Denot) -- lambdas     (a -> b)
           | Ints (S -> Denot)     -- intensions  (s -> a)

---eqlDenot :: Denot -> Denot -> Bool
instance Eq Denot where
      (==) (Indv s1) (Indv s2) 
            | s1 == s2 = True
            | otherwise = False
      (==) (TVal b1) (TVal b2) 
            | b1 == b2 = True
            | otherwise = False
instance Show Denot where
      show (Indv x) = x
      show (TVal t) = show t


--- function to check for equivalence of denotations 
--- doesn't check for function types because it can't 
--- doesn't check for intensions because it'll require evaluation 
--- therefore that is dealt with in the evaluator itself

data LExpr = LCon String | LVar Char -- constants and vars (any)
           | Lmbd LExpr LExpr        -- lambda expr        (a -> b)
           | Appl LExpr LExpr        -- application        (any)
           | Eql  LExpr LExpr        -- equal              (t)
           | Not  LExpr
           | And  LExpr LExpr | Or LExpr LExpr
           | Eqv LExpr LExpr  | Impl LExpr LExpr -- binary ops   (t)
           | Forall LExpr LExpr
           | Exists LExpr LExpr      -- quantifications    (t)
           | Necs LExpr              -- necessary          (t)
           | Futr LExpr | Past LExpr -- tenses             (t)
           | Intn LExpr | Extn LExpr -- intension, extension (s->a, a)
                deriving Eq

-- f :: LExpr -> Denot
-- [only for constants -> intensions; see hardcode]

show' :: LExpr -> String
show' (LCon x)      = x
show' (LVar x)      = [x]
show' (Lmbd x y)    = concat [ "λ", show' x,  "[", show' y, "]" ]
show' (Appl x y)    = concat [ show' x, "(", show' y, ")" ]
show' (Eql x y)     = concat [ show' x, "=", show' y ]
show' (Not x)       = concat [ "¬", show' x ]
show' (And x y)     = concat [ show' x, "∧", show' y ]
show' (Or x y)      = concat [ show' x, "∨", show' y ]
show' (Eqv x y)     = concat [ show' x, "↔", show' y ]
show' (Impl x y)    = concat [ show' x, "→", show' y ]
show' (Forall x y)  = concat [ "∀", show' x, "[", show' y, "]" ]
show' (Exists x y)  = concat [ "∃", show' x, "[", show' y, "]" ]
show' (Necs x)      = concat [ "□", "[", show' x, "]"]
show' (Futr x)      = concat [ "\\e[1mF\\e[0m", "[", show' x, "]" ]
show' (Past x)      = concat [ "\\e[1mP\\e[0m", "[", show' x, "]" ]
show' (Intn x)      = concat [ "^", "[", show' x, "]" ]
show' (Extn x)      = concat [ "ˇ", "[", show' x, "]" ]

instance Show LExpr where
  show x = show' x

type VarAssmt = LExpr -> Denot -- variables -> extensions
-----------------------

-- Interpretation --
type Model = ([Denot], [World], [Time], LExpr -> Denot)
           -- (A,       I,       J,       F)

-- Hardcode (p. 133-34) --
i :: [World]
i = [W 1, W 2]

j :: [Time]
j = [T 1, T 2]

--------------------------

-- Examples --
-- "John walks"

-- -> (F4 (Tm "John") (IV "walk"))

-- -> (Appl (Lmbd (LVar 'P')
--                (Appl (Extv (LVar 'P'))
--                      (LCon "j")))
--          (Intn (LCon "walk")))
--------------


--- Helper functions

--- or's a list of TVal's
orTValList :: [Denot] -> Denot
orTValList [] = TVal False
orTValList (x:xs) = TVal(bool_x || bool_tail)
    where TVal bool_x = x
          TVal bool_tail = orTValList xs

andTValList :: [Denot] -> Denot
andTVAlList [] = TVal False
andTValList (x:xs) = TVal(bool_x && bool_tail)
      where TVal bool_x = x
            TVal bool_tail = orTValList xs
