module Evaluator where

import AllTypes

eqlIntension :: Denot -> Denot -> Model -> Bool
eqlIntension (Ints a1) (Ints a2) model
      | orTValList [TVal ((a1 (w, t)) == (a2 (w, t)))
        | t <- time_list_model, w <- world_list_model ] == TVal True = True
      | otherwise = False
      where (_, _, time_list_model, _) = model
            (_, world_list_model, _, _) = model



eval :: LExpr -> Model -> VarAssmt -> Index -> Denot -- evaluate in model

--- Rule 1:
eval l@(LCon con) model g index = int_func  ((fst index), (snd index))
                                    where Ints int_func = (f l)

--- Rule 2: 
eval (LVar var) model g index = g (LVar var)


--- Rule 3:
eval (Lmbd (LVar u) alpha) model g index 
    = Func (\x -> eval alpha model (\(LVar u') -> if u' == u then x else g (LVar u')) index) 


--- Rule 4:
eval (Appl alpha beta) model g index = func (eval beta model g index)
                                where Func func = eval alpha model g index

--- Rule 5:
eval (Eql alpha beta) model g index
    | let Indv str_alpha = eval_alpha
          Indv str_beta = eval_beta
        in (str_alpha == str_beta) = TVal True
    | let TVal b1 = eval_alpha
          TVal b2 = eval_beta
        in (b1 == b2) = TVal True
    | let Ints b1 = eval_alpha
          Ints b2 = eval_beta
        in(eqlIntension eval_alpha eval_beta model) == True = TVal True
    | otherwise = TVal False
    where eval_alpha = eval alpha model g index
          eval_beta = eval beta model g index

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

--- Rule 11
eval (Forall (LVar u) phi) model g index
    | andTValList [eval phi model g' index | g' <- list_of_g] == TVal True = TVal True
    | otherwise = TVal False
    where list_of_g = [\(LVar u') -> if (u' == u) then ind else (g (LVar u')) | ind <- individual_list]
          (individual_list, _, _, _) = model 

--- Rule 12
eval (Exists (LVar u) phi) model g index
    | orTValList [eval phi model g' index | g' <- list_of_g] == TVal True = TVal True
    | otherwise = TVal False
    where list_of_g = [\(LVar u') -> if (u' == u) then ind else (g (LVar u')) | ind <- individual_list]
          (individual_list, _, _, _) = model 

--- Rule 13
eval (Necs phi) model g index
    | andTValList [eval phi model g (w, t) | t <- times, w <- worlds] == TVal True = TVal True
    | otherwise = TVal False
    where (_, worlds, times, _) = model

--- Rule 14:
eval (Futr phi) model g index
    | orTValList [eval phi model g ((fst index), t') 
                   | t' <- time_list_model, t' > (snd index)] == TVal True = TVal True
    | otherwise = TVal False
    where (_, _, time_list_model, _) = model 


--- Rule 15
eval (Past phi) model g index
    | orTValList [eval phi model g ((fst index), t')
                    | t' <- time_list_model, t' < (snd index)] == TVal True = TVal True
    | otherwise = TVal False
    where(_, _, time_list_model, _) = model

-- --- Rule 16
-- eval (Intn alpha) model g index
--     --- the Intn is a constructor of LExpr above

--- Rule 17
eval (Extn alpha) model g index = ints_alpha index
    where Ints ints_alpha = eval alpha model g index

eval _ _ _ _ = Indv ""
