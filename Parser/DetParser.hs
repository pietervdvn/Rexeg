module Parser.DetParser where

import Data.Char

parse 			:: DParser a -> (String, Int) -> Maybe (String, Int, a)
parse (DParser f)	=  f

extract			:: Maybe (String, Int, a) -> a
extract	(Just (_, _, a))= a
extract Nothing		= error "Parsing failed"

newtype DParser a = DParser ((String, Int) -> Maybe (String, Int, a))

instance Monad DParser where
	return a 		= DParser (\ (str, i) ->  Just (str, i, a))
	(>>=) prs1 fprs2	= DParser (\ (str, i) -> andThen (parse prs1 (str, i)) fprs2)

andThen				:: Maybe (String, Int, a) -> (a -> DParser b) -> Maybe (String, Int, b)
andThen Nothing _		=  Nothing
andThen (Just (str, i, a)) prs2	=  parse (prs2 a) (str, i)
						
abort			:: DParser a	
abort			=  DParser $ const Nothing
								
continue		:: DParser ()
continue		=  return ()	

continueIf		:: DParser a -> Bool -> DParser a
continueIf prs b	=  if b then prs else abort
								
failIf cond msg		=  if cond then fail msg else continue

_done			:: DParser ()
_done			=  DParser $ \ (str, i) -> if null str then Just (str,i,()) else Nothing

resting			:: DParser String
resting			=  DParser $ \(str, i) -> Just (str, i, str)
				
index			:: DParser Int
index			=  DParser $ \(str, i) -> Just (str, i, i)

full			:: DParser a -> DParser a
full prs		=  do 	a <- prs
				_done
				return a 


{-| Gives the first parser, if this one has a result |-}
(>:)			:: DParser a -> DParser a -> DParser a
(>:) prs backup		=  DParser( \ si -> _pref (parse prs si) backup si )
				where 	_pref		:: Maybe (String, Int, a) -> DParser a -> (String, Int) -> Maybe (String, Int, a)
					_pref Nothing p si	=  parse p si
					_pref (Just res) p _	=  Just res
						
(>:>)			:: [DParser a] -> DParser a
(>:>) (r:rs)		=  foldl (>:) r rs

char	 		:: DParser Char
char			= DParser _getChar
			where
				_getChar	:: (String, Int) -> Maybe (String, Int, Char)
				_getChar (c:cs, i) 	= Just (cs, i+1, c)
				_getChar ([], i)	= Nothing

toStr			:: DParser Char -> DParser String
toStr prs		=  do 	c <- prs
				return [c]

string			:: Int -> DParser String
string l		=  DParser _getString
			where
				_getString		:: (String, Int) -> Maybe (String, Int, String)
				_getString (str, i)	=  if length str >= l then Just (drop l str, i+l, take l str)
								else Nothing


charIf		:: (Char -> Bool) -> DParser Char
charIf b	=  do	c <- char
			if b c then return c else abort
			
charEq		:: Char -> DParser Char
charEq c	=  charIf (c==)

digit 	:: DParser Int			
digit	=  do 	c <- char
		if isDigit c then
			return (ord c - ord '0') else abort


integer	:: DParser Integer
integer	= do 	i <- int
		return $ fromIntegral i

int		:: DParser Int
int		=   do 	charEq '-'
			i <- posInt
			return (-i)
		 >: posInt

posInt	:: DParser Int
posInt	= do	d <- digit
		_int d

_int		:: Int -> DParser Int
_int acc 	=  do	dig <- digit
			_int (dig + acc*10)
		>: return acc

float		:: DParser Float
float		=  do	charEq '-'
		   	f <- posFloat
		   	return (-f)
		   >: posFloat
		
posFloat 	:: DParser Float
posFloat 	=  (do
			pre <- posInt
			charIf (`elem` ".,") 
			post <- posInt
			return $ _float pre post)
		>: (do 	i <- posInt
			return $ fromIntegral i)
			
_float		:: Int -> Int -> Float
_float pre post	=  pre' + post'		
	 	  where pre' = fromIntegral pre
			post' = (fromIntegral post) / (10^ length ( show post))
		
stringEq		:: String -> DParser String
stringEq		=  _fixedString ""

_fixedString 		:: String -> String -> DParser String
_fixedString acc (c:cs)	=  do 	_ <- charEq c
				_fixedString (acc++[c]) cs
_fixedString acc []	=  return acc

{- | pair matches the opening string, parses with the intermediate parser, and the closing string. e.g. for parens: pair "(" ")" yourParser|-}
pair			:: String -> String -> DParser a -> DParser a
pair open close prs	=  do	stringEq open
				a <- prs
				stringEq close
				return a
