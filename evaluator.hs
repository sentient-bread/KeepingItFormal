import AllTypes

eval :: LExpr -> Model -> VarAssmt -> Index -> Denot -- evaluate in model

--- Rule 1:
eval l@(LCon con) model g index = int_func  ((fst index), (snd index))
                                    where Ints int_func = (f l)

--- Rule 2: 
eval (LVar var) model g index = g (LVar var)


--- Rule 3:


--- Rule 4:
eval (Appl alpha beta) model g index = func (eval beta model g index)
                                where Func func = eval alpha model g index

--- Rule 5:
eval (Eql alpha beta) model g index
    | (int_alpha == int_beta) = TVal True 
    | otherwise = TVal False
    where int_alpha = eval alpha model g index
          int_beta = eval beta model g index

--- Rule 6:
eval (Not phi) model g index
    | eval phi model g index == TVal False = TVal True
    | otherwise = TVal True

--- Rule 7 (and):
eval (And phi psy) model g index
    | int_phi_truth && int_psy_truth == True = TVal True
    | otherwise = TVal False 
    where TVal int_phi_truth = eval phi model g index
          TVal int_psy_truth = eval phi model g index

--- Rule 8 (or):
eval (Or phi psy) model g index
    | int_phi_truth || int_psy_truth == True = TVal True
    | otherwise = TVal False
    where TVal int_phi_truth = eval phi model g index
          TVal int_psy_truth = eval psy model g index

--- Rule 9 (implication):
eval (Impl phi psy) model g index
    | (not int_phi_truth) || int_psy_truth == True = TVal True
    | otherwise = TVal False
    where TVal int_phi_truth = eval phi model g index
          TVal int_psy_truth = eval psy model g index

--- Rule 10 (double implication):
eval (Eqv phi psy) model g index
    | fwd_implication && bcwd_implication == True = TVal True
    | otherwise = TVal False
    where TVal fwd_implication = eval (Impl phi psy) model g index
          TVal bcwd_implication = eval (Impl psy phi) model g index 

--- Rule 14:
eval (Futr phi) model g index
    | orTValList [eval phi model g ((fst index), t') 
                   | t' <- time_list_model, t' > (snd index)] == TVal True = TVal True
    | otherwise = TVal False
    where (_, _, time_list_model, _) = model 
