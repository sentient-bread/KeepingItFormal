module Model where

import AllTypes

g :: VarAssmt
g _ = Indv "j"

model :: Model
model = (indivs, i, j, f)

indivs :: [Denot]
indivs = [Indv "j", Indv "m", Indv "b"]

f :: LExpr -> Denot
f (LCon "j") = Ints $ (\(_,_) -> Indv "j")
f (LCon "m") = Ints $ (\(_,_) -> Indv "m")
f (LCon "b") = Ints $ (\(_,_) -> Indv "b")
f (LCon "man") = Ints $ (\(w,t) -> case (w,t) of
                   (W 1, T 1) -> Func (\(Ints i) -> case (i (W 1, T 1)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal True)
                   (W 1, T 2) -> Func (\(Ints i) -> case (i (W 1, T 2)) of
                                   Indv "j" -> TVal False
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal True)
                   (W 2, T 1) -> Func (\(Ints i) -> case (i (W 2, T 1)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal True)
                   (W 2, T 2) -> Func (\(Ints i) -> case (i (W 2, T 2)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal True))
f (LCon "woman") = Ints $ (\(w,t) -> case (w,t) of
                     (W 1, T 1) -> Func (\(Ints i) -> case (i (W 1, T 1)) of
                                     Indv "j" -> TVal False
                                     Indv "m" -> TVal True
                                     Indv "b" -> TVal False)
                     (W 1, T 2) -> Func (\(Ints i) -> case (i (W 1, T 2)) of
                                     Indv "j" -> TVal True
                                     Indv "m" -> TVal True
                                     Indv "b" -> TVal False)
                     (W 2, T 1) -> Func (\(Ints i) -> case (i (W 2, T 1)) of
                                     Indv "j" -> TVal False
                                     Indv "m" -> TVal True
                                     Indv "b" -> TVal False)
                     (W 2, T 2) -> Func (\(Ints i) -> case (i (W 2, T 2)) of
                                     Indv "j" -> TVal False
                                     Indv "m" -> TVal True
                                     Indv "b" -> TVal False))
f (LCon "talk") = Ints $ (\(w,t) -> case (w,t) of
                   (W 1, T 1) -> Func (\(Ints i) -> case (i (W 1, T 1)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal True)
                   (W 1, T 2) -> Func (\(Ints i) -> case (i (W 1, T 2)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal True
                                   Indv "b" -> TVal True)
                   (W 2, T 1) -> Func (\(Ints i) -> case (i (W 2, T 1)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal False)
                   (W 2, T 2) -> Func (\(Ints i) -> case (i (W 2, T 2)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal True))
f (LCon "walk") = Ints $ (\(w,t) -> case (w,t) of
                   (W 1, T 1) -> Func (\(Ints i) -> case (i (W 1, T 1)) of
                                   Indv "j" -> TVal False
                                   Indv "m" -> TVal True
                                   Indv "b" -> TVal False)
                   (W 1, T 2) -> Func (\(Ints i) -> case (i (W 1, T 2)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal False
                                   Indv "b" -> TVal False)
                   (W 2, T 1) -> Func (\(Ints i) -> case (i (W 2, T 1)) of
                                   Indv "j" -> TVal False
                                   Indv "m" -> TVal True
                                   Indv "b" -> TVal True)
                   (W 2, T 2) -> Func (\(Ints i) -> case (i (W 2, T 2)) of
                                   Indv "j" -> TVal True
                                   Indv "m" -> TVal True
                                   Indv "b" -> TVal False))
f (LCon "see") = Ints $ (\(w,t) -> case (w,t) of
                   (W 1, T 1) -> Func (\(Ints i) -> case (i (W 1, T 1)) of
                                   Indv "j" -> Func (\(Ints j) -> case (j (W 1, T 1)) of
                                                 Indv "j" -> TVal False
                                                 Indv "m" -> TVal True
                                                 Indv "b" -> TVal True)
                                   Indv "m" -> Func (\(Ints j) -> case (j (W 1, T 1)) of
                                                 Indv "j" -> TVal True
                                                 Indv "m" -> TVal False
                                                 Indv "b" -> TVal True)
                                   Indv "b" -> Func (\(Ints j) -> case (j (W 1, T 1)) of
                                                 Indv "j" -> TVal False
                                                 Indv "m" -> TVal False
                                                 Indv "b" -> TVal False))
                   (W 1, T 2) -> Func (\(Ints i) -> case (i (W 1, T 2)) of
                                   Indv "j" -> Func (\(Ints j) -> case (j (W 1, T 2)) of
                                                 Indv "j" -> TVal True
                                                 Indv "m" -> TVal False
                                                 Indv "b" -> TVal False)
                                   Indv "m" -> Func (\(Ints j) -> case (j (W 1, T 2)) of
                                                 Indv "j" -> TVal False
                                                 Indv "m" -> TVal True
                                                 Indv "b" -> TVal False)
                                   Indv "b" -> Func (\(Ints j) -> case (j (W 1, T 2)) of
                                                 Indv "j" -> TVal True
                                                 Indv "m" -> TVal True
                                                 Indv "b" -> TVal False))
                   (W 2, T 1) -> Func (\(Ints i) -> case (i (W 2, T 1)) of
                                   Indv "j" -> Func (\(Ints j) -> case (j (W 2, T 1)) of
                                                 Indv "j" -> TVal False
                                                 Indv "m" -> TVal True
                                                 Indv "b" -> TVal True)
                                   Indv "m" -> Func (\(Ints j) -> case (j (W 2, T 1)) of
                                                 Indv "j" -> TVal True
                                                 Indv "m" -> TVal False
                                                 Indv "b" -> TVal True)
                                   Indv "b" -> Func (\(Ints j) -> case (j (W 2, T 1)) of
                                                 Indv "j" -> TVal False
                                                 Indv "m" -> TVal False
                                                 Indv "b" -> TVal False))
                   (W 2, T 2) -> Func (\(Ints i) -> case (i (W 2, T 1)) of
                                   Indv "j" -> Func (\(Ints j) -> case (j (W 2, T 2)) of
                                                 Indv "j" -> TVal True
                                                 Indv "m" -> TVal False
                                                 Indv "b" -> TVal False)
                                   Indv "m" -> Func (\(Ints j) -> case (j (W 2, T 2)) of
                                                 Indv "j" -> TVal False
                                                 Indv "m" -> TVal True
                                                 Indv "b" -> TVal False)
                                   Indv "b" -> Func (\(Ints j) -> case (j (W 2, T 2)) of
                                                 Indv "j" -> TVal True
                                                 Indv "m" -> TVal True
                                                 Indv "b" -> TVal False)))
