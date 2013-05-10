module Regex.RegexParser (regex) where

import Data.Char
import Regex.RegexDef
import Regex.SimpleParsers
import Parser.DetParser


{-|

Parser: meanings:

nothing of this list: just this fixed char
\\		: following char is captured as fixed
<i>		(with 'i' a int) : 'i' chars matched at the current point
^		: begining of the string, "<0>"
<-i>		(with 'i' a int) : 'i' chars resting in the current string
$		: end of the string, "<-0>"
\n		: the newline character
\f		: the formfeed character
[a..z]		(with 'a' and 'z' a char, with 'a' w 'z') : range, will match all the chars between 'a' and 'z'.
[s]		(with 'a' a string of arbitrary length, without unescaped ".", "]" or "\". "\.", "\]" and "\\" are allowed): will match any of the chars in 'a'
[a..zADX..Z\\]	: ranges can be combined. This equals "[a..z]|A|D|[X..Z]|\\"

regex!		: negation of the following character or part. E.g. ![a-d] will match anything except [a-d]. Does not affect repeaters (such as *, {}, ...). Laws of de Morgan apply.
			ab! will be parsed as (ab)!.
			
ORDER OF OPERATIONS:

()
| & : these are right-associative: a|b&c|d = a|(b&(c|d))
!, +, ?, *, {} (postfix operators)
special chars, ranges []
normal chars
E.G: "ab|def!ghi{0,5}" = "(ab)|((def)!((ghi){0,5})"
|-}

{- Regex parses the entire regex
Pass1 is responsible of the simple stuff: fixed and special chars (^, #, a, \\n, \\], ...)
Pass2 handles postfix operators (!, {0,42}, ?, +, ...)
Pass3 handles & and |
Pass4 (= regex handles) ()
-}

regex		:: DParser Regex
regex		=  _regex []

_regex		:: [Regex] -> DParser Regex
_regex acc	=  do	reg1 <- regex'
			_regex (reg1:acc) >: return (_unSeq $ reverse $ reg1:acc)

regex'		:: DParser Regex
regex'		=  sq >: pass3

sq		:: DParser Regex
sq		=  do	reg <- pair "(" ")" regex
			_tryPFes reg

pass3		:: DParser Regex
pass3		=  do	reg <- pass2
			logicOp '|' Or [reg] >: logicOp '&' And [reg] >: return reg

logicOp		:: Char -> ([Regex] -> Regex) -> [Regex] -> DParser Regex
logicOp c o acc	= do	charEq c
			reg <- regex
			logicOp c o (reg:acc) >: return ( o $ reverse $ reg:acc)

pass2		:: DParser Regex
pass2		=  _pass2 []

_pass2		:: [Regex] -> DParser Regex
_pass2 acc	=  do	rexs <- _pass2lst acc
			_pass2 rexs >: return ( _unSeq $ reverse rexs)
			
_pass2lst	:: [Regex] -> DParser [Regex]
_pass2lst acc	=  do 	reg <- pass1
			reg' <- _tryPFes reg
			return $ reg':acc
		
_tryPFes	:: Regex -> DParser Regex
_tryPFes reg	=  do	reg' <- pass2' reg
			_tryPFes reg'
		>: pass2' reg
		>: return reg
		

pass1		:: DParser Regex
pass1		=  do	reg1 <- pass1'
			_pass1 [reg1]
			

_pass1 		:: [Regex] -> DParser Regex
_pass1 acc	=  do	reg1 <- pass1'
			_pass1 (reg1:acc)
		>: return (_unSeq $ reverse acc)
		
_unSeq		:: [Regex] -> Regex
_unSeq [r]	=  r
_unSeq rs	=  Seq rs
