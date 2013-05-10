module Regex.RegexEval where

import Parser.Parser
import Regex.RegexDef

{-| Evaluates the given regex. Returns a list of tuples, with all possibilities. List of tuples is from the form: ( sring still to parsed, number of parsed chars, parsed string |-}
match			:: Regex -> Parser String
match Any		= toStr char
match Empty		= return ""
match Fail		= abort

match (Range c1 c2)	= toStr $ charIf (`elem` [c1..c2])
match (Fixed c)		= toStr $ charEq c
match (Seq rs)		= foldl (\p1 p2 -> do s1 <- p1; s2 <- p2; return (s1++s2)) (return "") ( map match rs )

match (Or rs)		= (??) (map match rs)
match (And rs)		= Parser.Parser.all (map match rs)
match (Invert (Fixed c))= toStr $ charIf (c/=)
match (Invert (Range c1 c2)) = toStr $ charIf (`notElem` [c1..c2])
match (Invert _)	= error "Invalid usage of not, try the optimizer to push 'Not' down to valid places"

match (BetweenTimes _ 0 r)	= return ""
match (BetweenTimes 0 j r)	= do	s <- match r
					sr <- match $ BetweenTimes 0 (j-1) r
					return (s++sr)
				? return ""
match (BetweenTimes i j r)	= do	s <- match r
					sr <- match $ BetweenTimes (i-1) (j-1) r
					return (s++sr)

match (MinTimes 0 r)	= match (MinTimes 1 r) ? return ""
match (MinTimes i r)	= do	s <- match r
				sr <- match $ MinTimes (i-1) r
				return (s++sr)
				
match (PosInString i)	= do	j <- index
				continueIf (return "") (i == j)
match (PosToEnd i)	= do	resting <- resting
				continueIf (return "") (i == length resting)
