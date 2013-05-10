module Parser.Parser where

import Data.Char
import qualified Parser.DetParser as Det

parse 			:: Parser a -> (String, Int) -> [(String, Int, a)]
parse (Parser f)	=  f

extract			:: (String, Int, a) -> a
extract	(_, _, a)	= a

newtype Parser a = Parser ((String, Int) -> [(String, Int, a)])

instance Monad Parser where
	return a 		= Parser (\ (str, i) ->  [(str, i, a)])
	(>>=) prs1 fprs2	= Parser (\ (str, i) -> concat [parse (fprs2 a) (str2, j) |
								(str2, j, a) <- parse prs1 (str, i)])
				
				
undet			:: Det.DParser a -> Parser a
undet	prs		=  Parser $ \ si -> _undetermin (Det.parse prs si)
				where 	_undetermin		:: Maybe (String, Int, a) -> [(String, Int, a)]
					_undetermin Nothing	=  []
					_undetermin (Just res)	=  [res]				
								
continue		:: Parser ()
continue		=  return ()	

continueIf		:: Parser a -> Bool -> Parser a
continueIf prs b	=  if b then prs else abort
								
abort			:: Parser a	
abort			= Parser $ const []

failIf cond msg		=  if cond then fail msg else continue

_done			:: Parser ()
_done			=  Parser $ \ (str, i) -> [(str,i,()) | null str]

resting			:: Parser String
resting			=  Parser $ \(str, i) -> [(str, i, str)]

longest			:: Parser a -> Parser a
longest prs 		=  Parser $ \ si -> if null $ parse prs si then []
						else [_longest $ parse prs si]

_longest		:: [(String, Int, a)] -> (String, Int, a)
_longest [(s, i, a)]	=  (s, i, a)
_longest ((s, i, a):rs)	=  if i >= i2 then (s, i, a)
				else (s2, i2, a2)
				where (s2, i2, a2) = _longest rs
				
index			:: Parser Int
index			=  Parser $ \(str, i) -> [(str, i, i)]

full			:: Parser a -> Parser a
full prs		=  do 	a <- prs
				_done
				return a 

{-| both parsers are executed, and return both results |-}
(?)			:: Parser a -> Parser a -> Parser a
(?) p1 p2		=  Parser ( \(str, i) ->parse p1 (str, i) ++ parse p2 (str, i))

(??)			:: [ Parser a] -> Parser a
(??)			=  foldl (?) abort

{-| Gives the first parser, if this one has (at least) one result |-}
(>:*)			:: Parser a -> Parser a -> Parser a
(>:*) prs backup	=  Parser (\ si -> if null (parse prs si) then parse backup si
						else parse prs si)
						
						
						
(>:>*)			:: [Parser a] -> Parser a
(>:>*) (r:rs)		=  foldl (>:*) r rs

{-| both parsers are executed and return if both have a result|-}
both			:: Parser a -> Parser a -> Parser a
both p1 p2		=  Parser (\ si -> if null (parse p1 si) || null (parse p2 si) then []
							else parse p1 si ++ parse p2 si)
					
all			:: [Parser a] -> Parser a
all (p1:p2:ps)		=  foldl both p1 (p2:ps) 
all _			=  error "At least two parsers are needed for all"

char	 		:: Parser Char
char			= Parser _getChar
			where
				_getChar	:: (String, Int) -> [(String, Int, Char)]
				_getChar (c:cs, i) 	= [(cs, i+1, c)]
				_getChar ([], i)	= []

toStr			:: Parser Char -> Parser String
toStr prs		=  do 	c <- prs
				return [c]

string			:: Int -> Parser String
string l		=  Parser _getString
			where
				_getString		:: (String, Int) -> [(String, Int, String)]
				_getString (str, i)	=  [(drop l str, i+l, take l str) | length str >= l]


charIf		:: (Char -> Bool) -> Parser Char
charIf b	=  do	c <- char
			if b c then return c else abort
			
charEq		:: Char -> Parser Char
charEq c	=  charIf (c==)

digit 	:: Parser Int			
digit	=  do 	c <- char
		if isDigit c then
			return (ord c - ord '0') else abort


int		:: Parser Int
int		=  posInt ? 
	   	   do 	charEq '-'
			i <- posInt
			return (-i)

posInt	:: Parser Int
posInt	= do	d <- digit
		_int d

integer	:: Parser Integer
integer	= do 	i <- int
		return $ fromIntegral i

_int		:: Int -> Parser Int
_int acc 	=  do	dig <- digit
			_int (dig + acc*10)
		? return acc

float		:: Parser Float
float		=  posFloat ? 
		   do	charEq '-'
		   	f <- posFloat
		   	return (-f)
		
posFloat 	:: Parser Float
posFloat 	=  (do
			pre <- posInt
			charIf (`elem` ".,") 
			post <- posInt
			return $ _float pre post)
		? (do 	i <- posInt
			return $ fromIntegral i)
			
_float		:: Int -> Int -> Float
_float pre post	=  pre' + post'		
	 	  where pre' = fromIntegral pre
			post' = (fromIntegral post) / (10^ length ( show post))
		
stringEq		:: String -> Parser String
stringEq		=  _fixedString ""

_fixedString 		:: String -> String -> Parser String
_fixedString acc (c:cs)	=  do 	_ <- charEq c
				_fixedString (acc++[c]) cs
_fixedString acc []	=  return acc

{- | pair matches the opening string, parses with the intermediate parser, and the closing string. e.g. for parens: pair "(" ")" yourParser|-}
pair			:: String -> String -> Parser a -> Parser a
pair open close prs	=  do	stringEq open
				a <- prs
				stringEq close
				return a
