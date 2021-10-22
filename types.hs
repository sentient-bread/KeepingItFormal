-- English --
data Eng = CN String      | Tm String  | Pron Int | IV String
        -- (common nouns)   (terms)      (pronouns) (intrans. v)
         | IAV String | TV String      | StV String
        -- (adverbs)    (trans. verbs)   (sent-taking verbs)
         | ItV String             | SmA String |
        -- (intrans-taking verbs)   (sent-modifying adv)
         | Prep String -- basic categories
        -- (prepositions)
         | F0 Eng | F1 Eng | F2 Eng -- CNs (every, the, a)
        -- :: CN -> Tm
         | F3 Int Eng Eng -- CN, T ()
        -- :: Int -> CN -> T -> T (replace pronoun w. CN)
         | F4 Eng Eng -- Term, IV (S-V)
        -- :: Tm -> IV -> T (subject-verb)
         | F5 Eng Eng -- TV, Term (V-O); Prep Term (PP)
        -- :: TV -> Tm -> IV (verb-object)
        -- :: Prep -> Tm -> IAV (prep-object)
         | F6 Eng Eng -- StV, T ; ItV, IV
        -- :: StV -> T -> IV (belives/thinks that T)
        -- :: ItV -> IV -> IV (tries to IV)
         | F7 Eng Eng -- SmA, T
        -- :: SmA -> T -> T (necessarily, possibly T)
         | F8 Eng Eng | F9 Eng Eng -- Term, T, IV (and,or)
        -- :: a -> a -> a
         | F10 Int Eng Eng -- Term, CN ; Term, T ; Term , IVerb (sub)
         | F11 Eng Eng | F12 Eng Eng | F13 Eng Eng
         | F14 Eng Eng | F15 Eng Eng -- Term, IV (tenses S-V)
        -- :: Tm -> IV -> T
              deriving Show

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

data LExpr = LCon String | LVar Char -- constants and vars (any)
           | Lmbd LExpr LExpr        -- lambda expr        (a -> b)
           | Appl LExpr LExpr        -- application        (any)
           | Eql  LExpr LExpr        -- equal              (t)
           | Not  LExpr
           | And  LExpr LExpr | Or LExpr LExpr
           | Eqv LExpr LExpr  | Impl LExpr -- binary ops   (t)
           | Forall LExpr LExpr
           | Exists LExpr LExpr      -- quantifications    (t)
           | Necs LExpr              -- necessary          (t)
           | Futr LExpr | Past LExpr -- tenses             (t)
           | Intn LExpr | Extn LExpr -- intension, extension (s->a, a)
                deriving (Show, Eq)

-- f :: LExpr -> Denot
-- [only for constants -> intensions; see hardcode]

type VarAssmt = LExpr -> Denot -- variables -> extensions

g :: VarAssmt
g _ = Indv ""
-----------------------

-- Interpretation --
type Model = ([Denot], [World], [Time], LExpr -> Denot)
           -- (A,       I,       J,       F)

eval :: LExpr -> Model -> VarAssmt -> Index -> Denot -- evaluate in model
eval _ _ _ _ = Indv ""
--------------------

-- Hardcode (p. 133-34) --
m :: Model
m = (a,i,j,f)

a :: [Denot]
a = [Indv "a", Indv "b", Indv "c"]

i :: [World]
i = [W 1, W 2]

j :: [Time]
j = [T 1, T 2, T 3]

f :: LExpr -> Denot
f (LCon "j") = Ints $ \(_,_) -> Indv "a"
f (LCon "d") = Ints $ \(_,_) -> Indv "b"
f (LCon "n") = Ints $ \(_,_) -> Indv "c"
f (LCon "m") = Ints $ \(w,t) -> case (w,t) of
                  (W 1, T 1) -> Indv "a"
                  (W 1, T 2) -> Indv "b"
                  (W 1, T 3) -> Indv "c"
                  (W 2, T 1) -> Indv "c"
                  (W 2, T 2) -> Indv "c"
                  (W 2, T 3) -> Indv "b"
f (LCon "b") = Ints $ \(w,t) -> case (w,t) of
                  (W 1, T 1) -> Func (\e -> case e of
                                  Indv "a" -> TVal True
                                  Indv "b" -> TVal True
                                  _        -> TVal False)
                  (W 1, T 2) -> Func (\e -> case e of
                                  Indv "a" -> TVal True
                                  Indv "c" -> TVal True
                                  _        -> TVal False)
                  (W 1, T 3) -> Func (\e -> case e of
                                  Indv "b" -> TVal True
                                  Indv "c" -> TVal True
                                  _        -> TVal False)
                  (W 2, T 1) -> Func (\e -> case e of
                                  Indv "b" -> TVal True
                                  Indv "c" -> TVal True
                                  _        -> TVal False)
                  (W 2, T 2) -> Func (\e -> case e of
                                  Indv "a" -> TVal True
                                  _        -> TVal False)
                  (W 2, T 3) -> Func (\e -> case e of
                                  _        -> TVal True)
--------------------------

-- Examples --
-- "John walks"

-- -> (F4 (Tm "John") (IV "walk"))

-- -> (Appl (Lmbd (LVar 'P')
--                (Appl (Extv (LVar 'P'))
--                      (LCon "j")))
--          (Intn (LCon "walk")))
--------------
