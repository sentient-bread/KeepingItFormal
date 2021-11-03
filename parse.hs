module Parse where

import AllTypes
import Data.Char

scanparse :: String -> Eng
-- takes sentence and parses
scanparse text = let Just (tree, _, []) = parseT 0 (words text)
                 in tree

parseSent :: Int -> [String] -> Maybe (Eng, Int, [String])
-- simple sentences
parseSent n lex@(v:vs) | v `elem` smads
                          = case parseSent n vs of
                              Just (sent, i, rest) -> Just (F7 (SmA v) sent,
                                                            i, rest)
                              Nothing -> Nothing
                       | otherwise
                          = case (parseTmS n lex) of
                                Just (subj, i, ivrest)
                                 -> case (parseIV i ivrest) of
                                      Just (iv, j, rest, tense, neg)
                                         -> case (tense, neg) of 
                                              ("pres", False) -> Just ((F4 subj iv),
                                                                   j, rest)
                                              ("pres", True) -> Just ((F12 subj iv),
                                                                   j, rest)
                                              ("futr", False) -> Just ((F13 subj iv),
                                                                   j, rest)
                                              ("futr", True) -> Just ((F14 subj iv),
                                                                   j, rest)
                                              ("past", False) -> Just ((F15 subj iv),
                                                                   j, rest)
                                              ("past", True) -> Just ((F16 subj iv),
                                                                   j, rest)
                                      Nothing -> Nothing
                                Nothing -> Nothing
              
parseT :: Int -> [String] -> Maybe (Eng, Int, [String])
-- complex sentences (right associative)
parseT n lex = case (parseSent n lex) of
                 Just (sent, i, rest)
                  -> case rest of
                      ("and":rem) -> case (parseT i rem) of
                                      Just (oth,j,r) ->
                                        Just (F8 sent oth, j, r)
                                      Nothing -> Just (sent, i, rest)
                      ("or":rem) -> case (parseT i rem) of
                                      Just (oth,j,r) ->
                                        Just (F9 sent oth, j, r)
                                      Nothing -> Just (sent, i, rest)
                      _ -> Just (sent, i, rest)
                 Nothing -> Nothing

parseSTmS :: Int -> [String] -> Maybe (Eng, Int, [String])
-- simple terms in subj position
parseSTmS n lex = case lex of
                  ("he":rem)  -> Just (Pron n 'M', n+1, rem)
                  ("she":rem) -> Just (Pron n 'F', n+1, rem)
                  ("it":rem)  -> Just (Pron n 'N', n+1, rem)
                  (x:rem) -> if (x `elem` dets)
                             then case (parseCN n rem) of
                                    Just (cn, i, rest) -> Just (F2 (Det x) cn, i , rest)
                             else if (isUpper $ head x)
                                  then Just (Tm x, n, rem)
                                  else Nothing

parseTmS :: Int -> [String] -> Maybe (Eng, Int, [String])
-- compound terms in S. position
parseTmS n lex = case (parseSTmS n lex) of
                 Just (term, i, rest)
                  ->  case rest of
                       ("and":rem) -> case (parseTmS i rem) of
                                       Just (oth,j,r) -> Just (F8 term oth,
                                                                j, r)
                                       Nothing -> Just (term, i, rest)
                       ("or":rem) -> case (parseTmS i rem) of
                                       Just (oth,j,r) -> Just (F9 term oth,
                                                                j, r)
                                       Nothing -> Just (term, i, rest)
                       _ -> Just (term, i, rest)
                 Nothing -> Nothing

parseSTmO :: Int -> [String] -> Maybe (Eng, Int, [String])
-- simple terms in obj position
parseSTmO n lex = case lex of
                  ("him":rem)  -> Just (Pron n 'M', n+1, rem)
                  ("her":rem) -> Just (Pron n 'F', n+1, rem)
                  ("it":rem)  -> Just (Pron n 'N', n+1, rem)
                  (x:rem) -> if (x `elem` dets)
                             then case (parseCN n rem) of
                                    Just (cn, i, rest) -> Just (F2 (Det x) cn, i , rest)
                             else if (isUpper $ head x)
                                  then Just (Tm x, n, rem)
                                  else Nothing

parseTmO :: Int -> [String] -> Maybe (Eng, Int, [String])
-- compound terms in O. position
parseTmO n lex = case (parseSTmO n lex) of
                 Just (term, i, rest)
                  ->  case rest of
                       ("and":rem) -> case (parseTmO i rem) of
                                       Just (oth,j,r) -> Just (F8 term oth,
                                                                j, r)
                                       Nothing -> Just (term, i, rest)
                       ("or":rem) -> case (parseTmO i rem) of
                                       Just (oth,j,r) -> Just (F9 term oth,
                                                                j, r)
                                       Nothing -> Just (term, i, rest)
                       _ -> Just (term, i, rest)
                 Nothing -> Nothing

parseSIV :: Int -> [String] -> Maybe (Eng, Int, [String], String, Bool)
-- simple intrans. verbs
parseSIV n lex@(v:vs)
    | v == "will"      = if (vs /= [] && head vs == "not")
                         then let Just (iv, i, rest) = getVb (tail vs)
                              in Just (iv, i, rest, "futr", True)
                         else let Just (iv, i, rest) = getVb vs
                              in Just (iv, i, rest, "futr", False)
    | v == "has" || v == "have"
                       = if (vs /= [] && head vs == "not")
                         then let Just (iv, i, rest) = getVb (tail vs)
                              in Just (iv, i, rest, "past", True)
                         else let Just (iv, i, rest) = getVb vs
                              in Just (iv, i, rest, "past", False)
    | v == "does" || v == "do"
                       = if (vs /= [] && head vs == "not")
                         then let Just (iv, i, rest) = getVb (tail vs)
                              in Just (iv, i, rest, "pres", True)
                         else let Just (iv, i, rest) = getVb vs
                              in Just (iv, i, rest, "pres", False)
    | otherwise        = let Just (iv, i, rest) = getVb lex
                         in Just (iv, i, rest, "pres", False)
      where getVb (v:vs) | v `elem` iverbs  = Just (IV v, n, vs)
                         | v `elem` tverbs  = case (parseTmO n vs) of
                                               Just (obj, i, rest) -> Just (F5 (TV v) obj,
                                                                            i, rest)
                         | v `elem` stverbs = case (parseT n $ tail vs) of
                                               Just (comp, i, rest) -> Just (F6 (StV v) comp,
                                                                             i, rest)
                         | v `elem` itverbs = case (parseIV n $ tail vs) of
                                               Just (comp, i, rest, _, _) -> Just (F6 (ItV v) comp,
                                                                             i, rest)
                         | otherwise = Nothing

parseIV :: Int -> [String] -> Maybe (Eng, Int, [String], String, Bool)
-- compound intrans. verbs, advs
parseIV n lex = case (parseSIV n lex) of
                  Just (iv, i, rest, t, n)
                    -> case rest of
                        ("and":rem) -> case (parseSIV i rem) of
                                        Just (oth,j,r,_,_) -> Just (F8 iv oth,
                                                                       j, r, t, n)
                                        Nothing -> Just (iv, i, rest, t, n)
                        ("or":rem) -> case (parseSIV i rem) of
                                        Just (oth,j,r,_,_) -> Just (F9 iv oth,
                                                                       j, r, t, n)
                                        Nothing -> Just (iv, i, rest, t, n)
                        _ -> case (parseIAV i rest) of
                               Just (iav, j, rem) -> Just (F7 iav iv, j, rem, t, n)
                               Nothing -> Just (iv, i, rest, t, n)
                  Nothing -> Nothing

parseIAV :: Int -> [String] -> Maybe (Eng, Int, [String])
-- adverbs
parseIAV n [] = Nothing
parseIAV n lex@(v:vs) | v `elem` advs
                         = Just (IAV v, n, vs)
                      | v `elem` preps
                         = let Just (term, i, rest) = parseTmO n vs
                           in Just (F5 (Prep v) term, i, rest)
                      | otherwise = Nothing

parseCN :: Int -> [String] -> Maybe (Eng, Int, [String])
-- common nouns
parseCN n lex@(v:vs) | v `elem` cnouns
                        = if (vs /= [] && head vs == "such")
                          then let Just (clause, i, rest) = parseT n (tail $ tail vs)
                               in Just (F3 n (CN v) clause, i, rest)
                          else Just (CN v, n, vs)
                     | otherwise = Nothing

getF10 :: [Int] -> (Char,Eng) -> Eng -> Eng
-- substitution
getF10 (n:ns) (g,term) e = F10 n term
                               (foldr ($) e'
                                      [replace (Pron i g) (Pron n ' ')
                                          | i <- ns])
                        where e' = (replace term (Pron n g) e)

replace :: Eng -> Eng -> Eng -> Eng
replace x y t = if (t == x) then y
                else case t of
                       F2 a b -> F2 (replace x y a)
                                    (replace x y b)
                       F3 n a b -> F3 n a
                                      (replace x y b)
                       F4 a b -> F4 (replace x y a)
                                    (replace x y b)
                       F5 a b -> F5 (replace x y a)
                                    (replace x y b)
                       F6 a b -> F6 (replace x y a)
                                    (replace x y b)
                       F7 a b -> F7 (replace x y a)
                                    (replace x y b)
                       F8 a b -> F8 (replace x y a)
                                    (replace x y b)
                       F9 a b -> F9 (replace x y a)
                                    (replace x y b)
                       F10 n a b -> F10 n a
                                        (replace x y b)
                       F11 a b -> F11 (replace x y a)
                                      (replace x y b)
                       F12 a b -> F12 (replace x y a)
                                      (replace x y b)
                       F13 a b -> F13 (replace x y a)
                                      (replace x y b)
                       F14 a b -> F14 (replace x y a)
                                      (replace x y b)
                       F15 a b -> F15 (replace x y a)
                                      (replace x y b)
                       F16 a b -> F16 (replace x y a)
                                      (replace x y b)
                       F17 a b -> F17 (replace x y a)
                                      (replace x y b)
                       _ -> t

cnouns :: [String]
cnouns = ["man", "woman", "unicorn"]

dets :: [String]
dets = ["every", "the", "a"]

smads :: [String]
smads = ["necessarily", "possibly"]

iverbs :: [String]
iverbs = ["talks", "walks", "talk", "walk", "talked", "walked"]

tverbs :: [String]
tverbs = ["sees", "seeks", "likes", "see", "seek", "like", "seen", "seeked", "liked"]

stverbs :: [String]
stverbs = ["believes", "thinks", "believe", "think", "believed", "thought"]

itverbs :: [String]
itverbs = ["tries", "attempts", "try", "attempt", "tried", "failed"]

advs :: [String]
advs = ["slowly", "rapidly"]

preps :: [String]
preps = ["in", "about"]
