{- jsmin.hs
   2008-08-03
Copyright (c) 2010 Jan Yac Matejka (yac@blesmrt.net)
Copyright (c) 2002 Douglas Crockford  (www.crockford.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

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
-- import Control.Monad.State

is_wsp x = elem x (map chr (range(0,9)++[11,12,127]++range(14,32)))
cr_or_lf x = elem x "\n\r"
is_alphanum x = elem x (map chr (range(48,57)++range(65,90)++range(97,122)))

wsp_keeper x
	| is_alphanum x = True
	| elem x "\\$_" = True
	| True = False

nl_pre_keeper x
	| is_alphanum x = True
	| elem x "\\$_}])+-\"'" = True
	| True = False

nl_post_keeper x
	| is_alphanum x = True
	| elem x "\\$_{[(+-" = True
	| True = False

re_pre_keeper x = elem x "(,=:[!&|?{};\n"


s_commentMultiEnd xs (y:[])
	| y == '/' = s_start xs " "
	| y == '*' = moar s_commentMultiEnd xs
	| True = moar s_commentMulti xs

s_commentMulti xs [] = putStr "Error: JSMIN Unterminated comment."
s_commentMulti xs (y:[])
	| y == '*' = moar s_commentMultiEnd xs
	| True = moar s_commentMulti xs

s_commentInline xs (y:[])
	| cr_or_lf y = s_start xs "\n"
	| True = moar s_commentInline xs

s_maybeCommentStart (xs) (y:[])
	| y == '/' = moar s_commentInline xs
	| y == '*' = moar s_commentMulti xs
	| re_pre_keeper (last xs) = s_regexp (xs++"/") [y]
	| True = s_start (xs++"/") [y]


s_maybeWSP xs ys 
	| is_wsp(head ys) = moar s_maybeWSP xs
	| wsp_keeper (head ys) = do
		putStr (xs++[' '])
		s_start [] ys
	| True = s_start xs ys

s_maybeNL xs (y:[])
	| cr_or_lf y || is_wsp y = moar s_maybeNL xs
	| nl_post_keeper y = do
		putStr (xs++"\n")
		moar s_start [y]
	| True = s_start xs [y]

s_maybeNL xs [] = do
	putStr xs

s_start xs [] = putStr xs
s_start xs ('/':[]) = moar s_maybeCommentStart xs
s_start [] (y:[])
	| is_wsp y || cr_or_lf y = moar s_start []
	| True = moar s_start [y]

s_start xs (y:[])
	| nl_pre_keeper (last xs) && cr_or_lf y = moar s_maybeNL xs
	| wsp_keeper (last xs) && is_wsp y = moar s_maybeWSP xs
	| is_wsp y || cr_or_lf y = moar s_start xs
	| y == '\'' = moar s_string1 (xs++[y])
	| y == '"' = moar s_string2 (xs++[y])
	| True = do 
		moar s_start (xs++[y])

s_string1 xs ('\\':[]) = moar s_string1free (xs++"\\")
s_string1 xs ('\'':[]) = moar s_start (xs++"'")
s_string1 xs (y:[]) = moar s_string1 (xs++[y])
s_string1 xs [] = putStrLn "Error: JSMIN unterminated string literal."

s_string2 xs ('\\':[]) = moar s_string2free (xs++"\\")
s_string2 xs ('"':[]) = moar s_start (xs++"\"")
s_string2 xs (y:[]) = moar s_string2 (xs++[y])
s_string2 xs [] = putStrLn "Error: JSMIN unterminated string literal."

s_regexp xs ('\\':[]) = moar s_regexpfree (xs++"\\")
s_regexp xs ('/':[]) = moar s_start (xs++"/")
s_regexp xs (y:[]) = moar s_regexp (xs++[y])
s_regexp xs [] = putStrLn "Error: JSMIN unterminated Regular Expression literal."

-- these are executed when escaping character is encountered to ignore next char if it was terminating w/o the escape
s_string2free xs (y:[]) = moar s_string2 (xs++[y])
s_string1free xs (y:[]) = moar s_string1 (xs++[y])
s_regexpfree xs (y:[]) = moar s_regexp (xs++[y])

moar state xs = do
	iseof <- IO.isEOF
	if iseof then state xs ""
		else do
			c <- getChar
			state xs [c]

main = do
	args <- getArgs
	if length(args) > 0
		then putStrLn ("// "++(head args))
		else putStr ""
	moar s_start [] 
