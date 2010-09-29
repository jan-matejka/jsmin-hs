module Main where
import System.Environment
import System
import IO
import Ix
import Char
import Control.Monad

isWSP x = elem x (filter (\x -> not(elem x "\r\n")) (chr 127:range(chr 0,chr 32)))
cRorLF x = elem x "\n\r"
asciiLetterOrDigit = map chr (range(33,47)++range(58,64)++range(91,96)++range(123,126))
asciiLetterOrDigitAnd xs = (filter (\x -> not(elem x xs)) asciiLetterOrDigit)

notWSPKeeper x = elem x ((asciiLetterOrDigitAnd "\\$_")++"\n\r")
notNLPreKeeper x = elem x ((asciiLetterOrDigitAnd "\\$_}])+-\"'")++range(chr 0,chr 32)) -- maybe this should also contain '/'
notNLPostKeeper x = elem x ((asciiLetterOrDigitAnd "\\$_{[(+-")++range(chr 0,chr 32))

preREKeeper x = elem x "(,=:[!&|?{};\n"

s_commentMultiEnd xs (y:[])
	| y == '/' = s_start xs " "
	| y == '*' = moar s_commentMultiEnd xs
	| True = moar s_commentMulti xs

s_commentMulti xs [] = putStr "Error: JSMIN Unterminated comment."
s_commentMulti xs (y:[])
	| y == '*' = moar s_commentMultiEnd xs
	| True = moar s_commentMulti xs

s_commentInline xs (y:[])
	| cRorLF y = s_start xs "\n"
	| True = moar s_commentInline xs

s_maybeCommentStart (xs) (y:[])
	| y == '/' = moar s_commentInline xs
	| y == '*' = moar s_commentMulti xs
	| preREKeeper (last xs) = s_regexp (xs++"/") [y]
	| True = s_start (xs++"/") [y]


s_maybeWSP xs ys 
	| isWSP(head ys) = moar s_maybeWSP xs
	| notWSPKeeper (head ys) = s_start xs ys
	| True = do
		putStr (xs++[' '])
		s_start [] ys

{- attempt to not have to distinguish strings  but it fails on /* in strings
shitfuck xs
	| isWSP (last xs) = shitfuck (init xs)
	| True = xs

s_maybeWSP xs (y:[])
	| isWSP  y = moar s_maybeWSP (xs++[y])
	| notWSPKeeper y = s_start (shitfuck xs) [y]
	| True = do
		putStr (xs)
		s_start [] [y]

-}

s_maybeNL xs (y:[])
	| cRorLF y || isWSP y = moar s_maybeNL xs
	| notNLPostKeeper y = s_start xs [y]
	| True = do
		putStr (xs++"\n")
		moar s_start [y]

s_maybeNL xs [] = do
	putStr xs

s_start xs [] = putStr xs
s_start xs ('/':[]) = moar s_maybeCommentStart xs
s_start [] (y:[])
	| isWSP y || cRorLF y = moar s_start []
	| True = moar s_start [y]

s_start xs (y:[])
	| not(notWSPKeeper (last xs)) && isWSP y = moar s_maybeWSP xs
	| not(notNLPreKeeper (last xs)) && cRorLF y = moar s_maybeNL xs
	| isWSP y || cRorLF y = moar s_start xs
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

s_string2free xs (y:[]) = moar s_string2 (xs++[y])
s_string1free xs (y:[]) = moar s_string1 (xs++[y])
s_regexpfree xs (y:[]) = moar s_regexp (xs++[y])

-- ^ major fail on "' in regexps

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
