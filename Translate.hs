module Translate where

import AllTypes

brace :: LExpr -> LExpr -> LExpr
-- brace application notation P{x}
brace f a = Extn (Appl f a)

star :: LExpr -> LExpr
-- star notation ∂*
star d = Lmbd (LVar 'y')
              (Lmbd (LVar 'x')
                    (Appl (Appl d
                                (Intn (Lmbd (LVar 'P')
                                            (brace (LVar 'P')
                                                   (LVar 'y')))))
                          (LVar 'x')))

translate :: Eng -> LExpr
translate (CN   x)           = find x
translate (Tm   x)           = Lmbd (LVar 'P') (brace (LVar 'P') (find x))
translate (IV   x)           = find x
translate (Pron n _)         = Lmbd (LVar 'P')
                                    (brace (LVar 'P')
                                           (LVar (head $ show n)))
translate (IAV  x)           = find x
translate (TV   x)           = find x
translate (StV  x)           = find x
translate (Det  x)           = find x
translate (ItV  x)           = find x
translate (SmA  x)           = find x
translate (Prep x)           = find x
translate (F2   d   cnoun)   = Appl (translate d) (Intn (translate cnoun))
translate (F3   n   cnoun t) = Lmbd (LVar (head $ show n))
                                    (And (Appl (translate cnoun)
                                               (LVar (head $ show n)))
                                         (translate t))
translate (F4   s   pred)    = Appl (translate s)   (Intn (translate pred))
translate (F5   v   obj)     = Appl (translate v)   (Intn (translate obj))
translate (F6   nec sent)    = Appl (translate nec) (Intn (translate sent))
translate (F7   adv vb)      = Appl (translate adv) (Intn (translate vb))
translate (F8   a   b)
                | (isSent a) = (And (translate a) (translate b))
                | otherwise  = Lmbd (LVar 'x')
                                    (And (Appl (translate a)
                                               (LVar 'x'))
                                         (Appl (translate b)
                                               (LVar 'x')))
translate (F9   a   b)
                | (isSent a) = (Or (translate a) (translate b))
                | otherwise  = Lmbd (LVar 'x')
                                    (Or (Appl (translate a)
                                              (LVar 'x'))
                                        (Appl (translate b)
                                              (LVar 'x')))
translate (F10  n   term t)  = Appl (translate term)
                                    (Intn (Lmbd (LVar (head $ show n))
                                                (translate t)))
translate (F11  v   sent)    =            Appl (translate v) (Intn (translate sent))
translate (F12  v   sent)    = Not       (Appl (translate v) (Intn (translate sent)))
translate (F13  v   sent)    =      Futr (Appl (translate v) (Intn (translate sent)))
translate (F14  v   sent)    = Not (Futr (Appl (translate v) (Intn (translate sent))))
translate (F15  v   sent)    =      Past (Appl (translate v) (Intn (translate sent)))
translate (F16  v   sent)    = Not (Past (Appl (translate v) (Intn (translate sent))))
translate (F17  v   comp)    = Appl (translate v) (Intn (translate comp))

isSent :: Eng -> Bool
isSent (F4  _ _)   = True
isSent (F6  _ _)   = True
isSent (F8  a _)   = isSent a
isSent (F9  a _)   = isSent a
isSent (F10 _ _ _) = True
isSent (F12 _ _)   = True
isSent (F13 _ _)   = True
isSent (F14 _ _)   = True
isSent (F15 _ _)   = True
isSent (F16 _ _)   = True
isSent  _          = False

find :: String -> LExpr -- constants
find "John"    = LCon "j"
find "Mary"    = LCon "m"
find "Bill"    = LCon "b"
find "man"     = LCon "man"
find "woman"   = LCon "woman"
find "unicorn" = LCon "uni"
find "talks"   = LCon "talk"
find "talk"    = LCon "talk"
find "walks"   = LCon "walk"
find "walk"    = LCon "walk"
find "sees"    = LCon "see"
find "see"     = LCon "see"
find "be"      = Lmbd (LVar 'P')
                      (Lmbd (LVar 'x')
                            (brace (LVar 'P')
                                   (Intn (Lmbd (LVar 'y')
                                               (Eql (LVar 'x')
                                                    (LVar 'y'))))))
find "every"   = Lmbd (LVar 'P')
                      (Lmbd (LVar 'Q')
                            (Forall (LVar 'x')
                                    (Impl (brace (LVar 'P')
                                                 (LVar 'x'))
                                          (brace (LVar 'Q')
                                                 (LVar 'x')))))
find "the"     = Lmbd (LVar 'P')
                      (Lmbd (LVar 'Q')
                            (Exists (LVar 'y')
                                    (And (Forall (LVar 'x')
                                                 (Eqv (brace (LVar 'P')
                                                             (LVar 'x'))
                                                      (Eql (LVar 'x')
                                                           (LVar 'y'))))
                                         (brace (LVar 'Q')
                                                (LVar 'x')))))
find "an"      = find "a"
find "a"       = Lmbd (LVar 'P')
                      (Lmbd (LVar 'Q')
                            (Exists (LVar 'x')
                                    (And (brace (LVar 'P')
                                                (LVar 'x'))
                                         (brace (LVar 'Q')
                                                (LVar 'x')))))
find "necessarily" = Lmbd (LVar 'p')
                          (Necs (Extn (LVar 'p')))
find "in"      = LCon "in"

simplify' :: LExpr -> LExpr
-- Intension is the "right inverse" of extension
simplify' (Extn (Appl (Intn x) y)) = Appl (simplify' x) (simplify' y)
-- Recursive simplification rules
simplify' (Appl (Lmbd (LVar x) body) y) = simplify' $ replace x y body
simplify' (Appl a b) = Appl (simplify' a) (simplify' b)
simplify' (Lmbd a b) = Lmbd (simplify' a) (simplify' b)
simplify' (Eql a b)  = Eql (simplify' a) (simplify' b)
simplify' (Not a) = Not (simplify' a)
simplify' (And a b)  = And (simplify' a) (simplify' b)
simplify' (Or a b)   = Or (simplify' a) (simplify' b)
simplify' (Eqv a b)  = Eqv (simplify' a) (simplify' b)
simplify' (Impl a b) = Impl (simplify' a) (simplify' b)
simplify' (Forall a b) = Forall (simplify' a) (simplify' b)
simplify' (Exists a b) = Exists (simplify' a) (simplify' b)
simplify' (Necs a) = Necs (simplify' a)
simplify' (Futr a) = Futr (simplify' a)
simplify' (Past a) = Past (simplify' a)
simplify' (Intn a) = Intn (simplify' a)
simplify' (Extn a) = Extn (simplify' a)
simplify' terminal = terminal

-- If there are multiple Apples nested on the outside, then
-- simplify' once is not sufficient; we must apply repeatedly till there
-- can be no further simplification
fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f p = until (\l -> l == f l) f p

simplify :: LExpr -> LExpr
simplify p = fixpoint simplify' p

-- replacement also needs to be recursive in order to traverse
-- the AST and find the appropriate terminal to replace
replace :: Char -> LExpr -> LExpr -> LExpr
replace x y body = case body of
                     (LVar a)
                       |  a == x          -> y
                       |  otherwise       -> body
                     (Appl a b)        -> Appl (replace x y a) (replace x y b)
                     (Lmbd a b)        -> Lmbd a (replace x y b)
                     (Eql a b)         -> Eql (replace x y a) (replace x y b)
                     (Not a)           -> Not (replace x y a)
                     (And a b)         -> And (replace x y a) (replace x y b)
                     (Or a b)          -> Or (replace x y a) (replace x y b)
                     (Eqv a b)         -> Eqv (replace x y a) (replace x y b)
                     (Impl a b)        -> Impl (replace x y a) (replace x y b)
                     (Forall a b)      -> Forall (replace x y a) (replace x y b)
                     (Exists a b)      -> Exists (replace x y a) (replace x y b)
                     (Necs a)          -> Necs (replace x y a)
                     (Futr a)          -> Futr (replace x y a)
                     (Intn a)          -> Intn (replace x y a)
                     (Extn a)          -> Extn (replace x y a)
                     terminal          -> terminal
