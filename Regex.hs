module Regex (regex, parseRegex, match) where

import qualified Parser.DetParser as DP
import qualified Parser.Parser as UP

import Regex.RegexDef
import Regex.RegexParser
import Regex.ShowRegex
import Regex.RegexEval
import Regex.Optimize

parseRegex	:: String -> Regex
parseRegex str	= _extract str $ DP.parse regex (str, 0)

_extract			:: String -> Maybe (String, Int, Regex) -> Regex
_extract str Nothing 		= error $ "Could not parse regex "++str
_extract str (Just (_,_,reg))	= reg
