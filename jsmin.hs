{- jsmin.hs
Copyright (c) 2010 Jan Yac Matejka (yac.blesmrt.net)
Copyright (c) 2002 Douglas Crockford  (www.crockford.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

The Software shall be used for Good, not Evil.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

module Main where
import System.Environment
import System
import IO
import Ix
import Char
import Control.Monad.State
import Maybe

is_wsp :: Char -> Bool
is_wsp x = elem x (map chr (range(0,9)++[11,12,127]++range(14,32)))

cr_or_lf :: Char -> Bool
cr_or_lf x = elem x "\n\r"

is_alphanum :: Char -> Bool
is_alphanum x = elem x (map chr (range(48,57)++range(65,90)++range(97,122)))

wsp_keeper :: Char -> Bool
wsp_keeper x
	| is_alphanum x = True
	| elem x "\\$_" = True
	| True = False

nl_pre_keeper :: Char -> Bool
nl_pre_keeper x
	| is_alphanum x = True
	| elem x "\\$_}])+-\"'" = True
	| True = False

nl_post_keeper :: Char -> Bool
nl_post_keeper x
	| is_alphanum x = True
	| elem x "\\$_{[(+-" = True
	| True = False

re_pre_keeper :: Char -> Bool
re_pre_keeper x = elem x "(,=:[!&|?{};\n"

consume :: String -> String
consume xs = xs

ommit :: String -> String
ommit xs = ""

until' :: String -> (String -> String) ->  (String,String) -> PartialParserResult
until' xs f (a,b)
	| length xs > length a = (a,b,False)
	| take (length xs) a == xs = (drop (length xs) a, b++(f xs), True)
	| True = until' xs f (tail a, b++(f [head a]))


type PartialParserResult = (String,String,Bool)
type PartialParser = (String,String) -> PartialParserResult 
type ParseState = (String, String, PartialParser)
--              buffer, result, parser

parse_code :: (String,String) -> PartialParserResult
parse_code ([],b) = ([],b,True)

parse_code (a,b) =
	case (head a) of
		'"' ->  parse_string (tail a, b++"\"STR")
		'\'' -> parse_string' (tail a, b++"'STR2")
		'/' ->
			case (take 2 a) of
				"//" -> parse_comment_inline (a,b++"CM")
				"/*" -> parse_comment_multiline (a,b++"CM2")
				_ -> parse_regexp (tail a,b++"RE")
		_ -> parse_code (tail a,b++[head a])

parse_regexp = until' "/" consume
parse_string' = until' "'" consume
parse_string :: (String,String) -> PartialParserResult
parse_string = until' "\"" consume
parse_comment_inline = until' "\n" ommit
parse_comment_multiline = until' "*/" ommit

parse :: State ParseState ParseState
parse = do
	(a,b,f) <- get
	let (a1,b1,finished) = f (a,b)
	if finished
		then return (a1,b1,parse_code)
		else do
			put (a1,b1,f)
			return (a1,b1,f)

parse2 (a,b,f) = do
	let (a1,b1,flag) = f (a,b)
	if flag
		then (a1,b1,f)
		else (a1,b1,parse_code)

moar :: ParseState -> IO ()
moar state = do
--	let (a,b,f) = evalState (parse) state
	let (a,b,f) = parse2 state
	putStr b
	iseof <- IO.isEOF
	if iseof
		then return ()
		else do
			l <- IO.getLine
			moar (a++l++"\n","",f)

main = do
	args <- getArgs
	if not (null args)
		then putStrLn ("// "++(head args))
		else putStr ""
	moar ("","",parse_code)
