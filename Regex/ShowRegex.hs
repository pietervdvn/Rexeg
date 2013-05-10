module Regex.ShowRegex where

import Regex.RegexDef
import Regex.SimpleParsers
instance Show Regex where
	show = shw
	
shw			:: Regex -> String
shw Empty		= "_"
shw Any			= "."
shw Fail		= "#"

shw (Range c1 c2)	= "[" ++ shw ( Fixed c1) ++".." ++ shw ( Fixed c2) ++ "]"
				
shw (Fixed '\n')	= "\\n"
shw (Fixed '\f')	= "\\f"
shw (Fixed '\t')	= "\\t"
shw (Fixed '_')		= "_"
shw (Fixed c)		= if c `elem` specialChars then '\\':[c]
				else [c]

shw (Seq rs)		= foldl (++) "(" (map shw rs) ++ ")"

shw (Or [])		= ""
shw (Or [r])		= shw r
shw (Or (r:rs))		= if allFixedOrRange (r:rs) 
				then foldl (++) "[" (map showSpecial (r:rs)) ++ "]"
				else "("++ foldl (\ a b -> a++"|"++b) (shw r) (map shw rs) ++")"

shw (And [])		= ""
shw (And [r])		= shw r
shw (And (r:rs))	= "("++ foldl (\ a b -> a++"&"++b) (shw r) (map shw rs) ++")"

shw (Invert r)		= shw r ++ "!"

shw (BetweenTimes 0 0 r) = ""
shw (BetweenTimes 0 1 r) = shw r ++ "?"
shw (BetweenTimes 1 1 r) = shw r
shw (BetweenTimes 0 j r) = shw r ++ "{,"++ show j ++"}"
shw (BetweenTimes i j r) = shw r ++ "{" ++ (if i == j then show i
				else show i ++ "," ++ show j) ++ "}"

shw (MinTimes 0 r)	= shw r ++ "*"
shw (MinTimes 1 r)	= shw r ++ "+"
shw (MinTimes i r)	= shw r ++ "{"++show i++",}"

shw (PosInString 0)	= "^"
shw (PosInString i)	= "<"++show i++">"
shw (PosToEnd 0)	= "$"
shw (PosToEnd i)	= "<-"++show i++">"

showSpecial			:: Regex -> String
showSpecial (Range c1 c2)	=  shw ( Fixed c1) ++ ".." ++ shw ( Fixed c2)
showSpecial r			=  shw r

allFixedOrRange				:: [Regex] -> Bool
allFixedOrRange (Fixed _:rs)		=  allFixedOrRange rs
allFixedOrRange (Range _ _:rs)	=  allFixedOrRange rs
allFixedOrRange (_:rs)			=  False
allFixedOrRange []			=  True
