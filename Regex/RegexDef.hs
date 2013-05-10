module Regex.RegexDef (Regex( Any, Empty, Fail,
			Range, Fixed, 
			Seq, Or, And, Invert,
			BetweenTimes, MinTimes,
			PosInString, PosToEnd),
		 Position) where

type Position = Int

data Regex =      Any 				-- any char, represented as "."; Not Any will fail	
		| Empty				-- does not consume a char, used for technical reasons. Represented as "" (empty string). Always matches. Not Empty == Empty
		| Fail				-- Fail: the regex will fail at this point, no matter what. Not Fail == Fail
		
		| Range Char Char 		-- Range: "[a..z]": matches one char that is between 'a' and 'z' in the ascii-table. [a..z]! will match any char <'a' and >'z'
		| Fixed Char 			-- Fixed char: "a", will match one char 'a'; Not "a" will match any char, except 'a'
		| Seq [Regex] 			-- Sequence: the list will be matched sequentially one after another
		| Or [Regex]			-- Or: Choose one option/proceed to try all options simultanously. Or [] == Fail
		| And [Regex] 			-- And: Try all the options. Continue if all succeed, on all tracks. And [] == Empty
		| Invert Regex 			-- Invert: inverts the affected chars: a! becomes all except a, (a|b|c)! == [a..c]! == all except these
		| BetweenTimes Int Int Regex				--  BetweenTimes: "{i,j}".
		| MinTimes Int Regex					-- MinTimes: at least so much times: "{i,}"
		| PosInString Position 		| PosToEnd Position	-- the position in the string from start ; and end. Does not consume a character. Does not work well together with the optimizer. Only use for research, or unoptimized...s
									-- PosToEnd 42 means that the user expects that there are another 42 chars before EOS)
	deriving (Eq)
	


