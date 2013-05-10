module Regex.Optimize (optimize) where

import Data.List
import Data.Char
import Regex.RegexDef

optimize	=  opt

opt		:: Regex -> Regex
opt (Seq rs)	=  failer $ optSeq $ Seq $ map optimize rs
opt (And rs)	=  failer $ optAnd $ And $ map optimize rs
opt (Or rs)	=  optOr'' $ map optimize rs
opt r		=  r

{- -}

optOrAsRange 		:: Regex -> Regex
optOrAsRange (Or rs)	=  if all oneCharConsumer rs then optRange rs else Or rs

optRange		:: [Regex] -> Regex
optRange regs		=  if contAny then Any
				else merge (combine rs cs) (combine nrs ncs)
				where (cs, ncs, rs, nrs, contAny) = extractChars' regs

type CharRanges	=  ([Char], [Char], [(Char, Char)], [(Char, Char)], Bool)

combine			:: [(Char, Char)] -> [Char] -> [Regex]
combine rs cs		=  chain $ sortRanges $ map (\(c1, c2) -> Range c1 c2) rs ++ map (\c -> Range c c) cs

sortRanges			:: [Regex] -> [Regex]
sortRanges []			=  []
sortRanges (Range a z:rs)	=  sortRanges [low | low <- rs, (\ (Range al zl) -> al < a) low]
					 ++ [(Range a z)] ++ 
				   sortRanges [high | high <- rs, (\ (Range al zl) -> al >= a) high]

merge			:: [Regex] -> [Regex] -> Regex
merge rs nrs		=  Or $ rs++nrs -- TODO

chain					:: [Regex] -> [Regex]
chain (Range a1 z1:Range a2 z2:cs)
	| z1 < a1			= error "Range: closing char is bigger then opening char. This is not supposed to happen"
	| a2 < a1			= chain (Range a2 z2:Range a1 z1:cs)
	| a1 <= a2
		 && ord a2 <= 1+ord z1	= if a1 <= z2 && z2 <= z1 
						then chain (Range a1 z1:cs)
						else chain (Range a1 z2:cs)
	| otherwise			= Range a1 z1:chain (Range a2 z2:cs)
chain range				= range
  
					

extractChars'	= extractChars ([],[],[],[],False)

extractChars							:: CharRanges -> [Regex] -> CharRanges
extractChars results		[]				=  results
extractChars (cs, ncs, rs, nrs, b) (Any:regs) 			=  (cs, ncs, rs, nrs, True)
extractChars (cs, ncs, rs, nrs, b) (Fixed c:regs) 		=  extractChars (c:cs, ncs, rs, nrs, b) regs
extractChars (cs, ncs, rs, nrs, b) (Invert (Fixed c):regs) 	=  extractChars (cs, c:ncs, rs, nrs, b) regs
extractChars (cs, ncs, rs, nrs, b) (Range r1 r2:regs) 	=  extractChars (cs, ncs, (r1, r2):rs, nrs, b) regs
extractChars (cs, ncs, rs, nrs, b) (Invert (Range r1 r2):regs) =  extractChars (cs, ncs, rs, (r1, r2):nrs, b) regs


-- optOr'' does all the work by delegating. optOr' will optimize the or, here range-optimizeation is called
optOr''		:: [Regex] -> Regex
optOr'' rs 	=  optOrAsRange $ optOr' rs

-- the actual optimization step, after removing empties
optOr'		:: [Regex] -> Regex
optOr' rs	=  optOr $ Or $ remEmptyAndAppend rs

-- Removes all Empties, and appends one at the front, if at least one empty was present
remEmptyAndAppend	:: [Regex] -> [Regex]
remEmptyAndAppend rs	=  if Empty `elem` rs then Empty:remove Empty rs else rs

optOr			:: Regex -> Regex
optOr (Or [])		=  Fail
optOr (Or [r])		=  r
optOr (Or (Fail:rs))	=  optOr $ Or rs
--optOr (Or (Empty:rs)	=  optOr $ Or rs --Empties are filtered out at this point, except a possible first one
optOr (Or (Or r:rs))	= optOr $ Or $ r++rs
optOr (Or (r:rs))	= addNE Or $ r: remOr ( optOr $ Or rs)

remove		:: Eq a => a -> [a] -> [a]
remove a []	=  []
remove a (r:rs)	=  if a==r 	then remove a rs
				else r:remove a rs
				
oneCharConsumer			:: Regex -> Bool
oneCharConsumer (Fixed _)	=  True
oneCharConsumer (Range _ _)	=  True
oneCharConsumer (Invert (Fixed _))	=  True
oneCharConsumer (Invert (Range _ _))	=  True
oneCharConsumer Any		= True
oneCharConsumer	_		= False

{- -}

optAnd			:: Regex -> Regex
optANd (And [])		=  Empty
optAnd (And [r])	=  r
optAnd (And (Fail:rs))	=  Fail
optAnd (And (Empty:rs)) =  optAnd $ And rs
optAnd (And (And r:rs)) =  optAnd (And $ r++rs)
optAnd (And (r:rs))	= addNE And $ r: remAnd ( optAnd $ And rs)

{-| Expects an Seq of optimized contents |-}
optSeq 			:: Regex -> Regex
optSeq (Seq [])		=  Empty
optSeq (Seq [r])	=  r
optSeq (Seq (Empty:rs))	=  optSeq $ Seq rs
optSeq (Seq (Fail:rs))	=  Fail
optSeq (Seq (Seq r:rs)) = optSeq $ Seq $ r ++ rs
optSeq (Seq (r:rs))	=  addNE Seq $ r: remSeq ( optSeq $ Seq rs )
optSeq r		=  r

failer			:: Regex -> Regex
failer (Seq rs)		=  if Fail `elem` rs then Fail else Seq rs
failer (And rs)		=  if Fail `elem` rs then Fail else And rs
failer r		=  r

remSeq			:: Regex -> [Regex]
remSeq (Seq [])		=  []
remSeq (Seq [r])	=  [r]
remSeq (Seq rs)		=  rs
remSeq r		=  [r]

remAnd (And [])		=  [Empty]
remAnd (And [r])	=  [r]
remAnd (And rs)		=  rs
remAnd r		=  [r]

remOr (Or [])		=  []
remOr (Or [r])		=  [r]
remOr (Or rs)		=  rs
remOr r		=  [r]

addNE			:: ([Regex] -> Regex) -> [Regex] -> Regex
addNE _ []		=  Empty
addNE _ [r]		=  r
addNE _ (r:Empty:[])	=  r
addNE cons rs		=  cons rs
