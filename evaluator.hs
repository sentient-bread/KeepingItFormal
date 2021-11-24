import AllTypes ( Model, VarAssmt, LExpr (LCon), Denot(Indv), Index ) 

eval :: LExpr -> Model -> VarAssmt -> Index -> Denot -- evaluate in model

--- Rule 1:
eval (LCon con) model g index = (f con ) ((fst index) (snd index))

--- Rule 2: 
eval (LVar var) model g index = g var


--- Rule 3:


--- Rule 4:
eval (Appl alpha beta) model g index = func (eval beta model g index)
                                where Func func = eval alpha model g index

--- Rule 5:
