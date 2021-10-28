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
                                      Just (iv, j, rest) -> Just ((F4 subj iv),
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

parseSIV :: Int -> [String] -> Maybe (Eng, Int, [String])
-- simple intrans. verbs
parseSIV n lex@(v:vs)
    | v `elem` iverbs  = Just (IV v, n, vs)
    | v `elem` tverbs  = case (parseTmO n vs) of
                           Just (obj, i, rest) -> Just (F5 (TV v) obj,
                                                         i, rest)
    | v `elem` stverbs = case (parseT n $ tail vs) of
                           Just (comp, i, rest) -> Just (F6 (StV v) comp,
                                                             i, rest)
    | v `elem` itverbs = case (parseIV n $ tail vs) of
                           Just (comp, i, rest) -> Just (F6 (ItV v) comp,
                                                         i, rest)
    | otherwise = Nothing

parseIV :: Int -> [String] -> Maybe (Eng, Int, [String])
-- compound intrans. verbs, advs
parseIV n lex = case (parseSIV n lex) of
                  Just (iv, i, rest)
                    -> case rest of
                        ("and":rem) -> case (parseSIV i rem) of
                                        Just (oth,j,r) -> Just (F8 iv oth,
                                                                j, r)
                                        Nothing -> Just (iv, i, rest)
                        ("or":rem) -> case (parseSIV i rem) of
                                        Just (oth,j,r) -> Just (F9 iv oth,
                                                                j, r)
                                        Nothing -> Just (iv, i, rest)
                        _ -> case (parseIAV i rest) of
                               Just (iav, j, rem) -> Just (F7 iav iv, j, rem)
                               Nothing -> Just (iv, i, rest)
                  Nothing -> Nothing

parseIAV :: Int -> [String] -> Maybe (Eng, Int, [String])
-- adverbs
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

getF10 :: [Int] -> Eng -> Eng -> Eng
-- substitution
getF10 (n:ns) cnoun e = F10 n cnoun (replace n cnoun ns e)

replace :: Int -> Eng -> [Int] -> Eng -> Eng
-- replacement
replace n cnoun ns t = if (t == cnoun) then Pron n ' '
                       else case t of
                              F2 d c -> F2 d (replace n cnoun ns c)
                              Pron i g -> if (i `elem` ns) then Pron n g
                                          else t 
                              F3 i c s -> F3 i c (replace n cnoun ns s)
                              F4 s p -> F4 (replace n cnoun ns s)
                                           (replace n cnoun ns p)
                              F5 v o -> F5 (replace n cnoun ns v)
                                           (replace n cnoun ns o)
                              F6 v t -> F6 (replace n cnoun ns v)
                                           (replace n cnoun ns t)
                              F7 a t -> F7 (replace n cnoun ns a)
                                           (replace n cnoun ns t)
                              F8 a b -> F8 (replace n cnoun ns a)
                                           (replace n cnoun ns b)
                              F9 a b -> F9 (replace n cnoun ns a)
                                           (replace n cnoun ns b)
                              _ -> t

cnouns :: [String]
cnouns = ["man", "woman", "unicorn"]

dets :: [String]
dets = ["every", "the", "a"]

smads :: [String]
smads = ["necessarily", "possibly"]

iverbs :: [String]
iverbs = ["talks", "walks", "talk", "walk"]

tverbs :: [String]
tverbs = ["sees", "seeks", "likes", "see", "seek", "like"]

stverbs :: [String]
stverbs = ["believes", "thinks"]

itverbs :: [String]
itverbs = ["tries", "attempts"]

advs :: [String]
advs = ["slowly", "rapidly"]

preps :: [String]
preps = ["in", "about"]
