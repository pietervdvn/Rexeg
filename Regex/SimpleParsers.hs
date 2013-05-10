module Regex.SimpleParsers where

import Parser.DetParser
import Regex.RegexDef
import Data.Char

-- | these are metacharacters and have to be used with a \ to catch them literally.
specialChars	:: String
specialChars	=  "{!&|()<>"

pass2'		:: Regex -> DParser Regex
pass2' r	= (>:>) $ map (\x -> x r) (times:postFix)

postFix		= map (\ (chr, meaning) r -> do charEq chr; return $ meaning r) postFixes

postFixes	= [('!', Invert), ('*', MinTimes 0), ('+', MinTimes 1), ('?', BetweenTimes 0 1)]

postFixChrs	= map fst postFixes
			
times		:: Regex -> DParser Regex
times r		=  pair "{" "}" (_times r)

_times r = do 	i <- int -- {i,j}
		charEq ','
		j <- int
		failIf (i < 0 || j < 0) $ "Failed parsing '{"++show i++","++show j++"}': positive numbers were expected"
		failIf (i > j)		$ "Failed parsing '{"++show i++","++show j++"}': the minimum should be smaller than the maximum"
		return $ BetweenTimes i j r
	>: do	i <- int --{i,}
		charEq ','
		failIf (i < 0) $ "Failed parsing '{"++show i++",}': a positive number was expected"
		return $ MinTimes i r
	>: do 	i <- int -- {i}
		failIf (i < 0) $ "Failed parsing '{"++show i++"}': a positive number was expected" 
		return $ BetweenTimes i i r
	>: do	charEq ','-- {,j}
		j <- int
		failIf (j < 0) $ "Failed parsing '{,"++show j++"}': a positive number was expected"
		return $ BetweenTimes 0 j r
	>: do	charEq ',' --{,}
		return $ MinTimes 0 r

pass1'		:: DParser Regex
pass1'		= (>:>) [specials, range, pos, fixed]

specials	:: DParser Regex
specials	=  (>:>) $ map (\(str, meaning) -> do 	stringEq str;
							return meaning)
			[("\\n", Fixed '\n'), ("\\f", Fixed '\f'),("\\t", Fixed '\t'), ("_", Empty), ("^" , PosInString 0), ("$" , PosToEnd 0), ("." , Any), ("#" , Fail)]

range		:: DParser Regex
range		=  pair "[" "]" _range


_range		= (do	r1 <- _rangeFT >: _rangeCharR
		   	r2 <- _range
		   	return $ Or (r1:[r2]))
		 >: _rangeFT
		 >: _rangeCharR

_rangeCharR	:: DParser Regex
_rangeCharR	=  do 	c <- _rangeChar
			return $ Fixed c
		 	
_rangeChar	:: DParser Char
_rangeChar	= do	charEq '\\'
			char
		>: charIf (`notElem` "].\\")

_rangeFT	:: DParser Regex
_rangeFT	=  do	start <- _rangeChar
			stringEq ".."
			stop <- _rangeChar
			failIf (ord start > ord stop) $ "Failed parsing '["++[start]++"-"++[stop]++"]': starting char if > ending char"
			return $ Range start stop
			

pos		:: DParser Regex
pos		=  pair "<" ">" 
			   ((do charEq '-'
				i <- posInt
				return $ PosToEnd i)
			>: (do 	i <- posInt
				return $ PosInString i))	
		
fixed		:: DParser Regex
fixed		=  do 	charEq '\\'
			c <- char
			return $ Fixed c
		 >: do	c <- charIf (`notElem` "|&{()"++postFixChrs)
		 	return $ Fixed c
