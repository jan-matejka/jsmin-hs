{- jsmin.hs
   2010-09-29
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
is_wsp x = elem (ord x) (range(0,9)++[11,12,127]++range(14,32))

cr_or_lf :: Char -> Bool
cr_or_lf x = elem x "\n\r"

is_alphanum :: Char -> Bool
is_alphanum x = elem (ord x) (range(48,57)++range(65,90)++range(97,122))

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

type Buffer = Char
type Result = String
type S_String_Type = Char -- " or '

data StateId = S_String S_String_Type
	| S_Code
	| S_Regexp
	| S_CommentInline
	| S_CommentMulti
	deriving (Show) -- for debug

type ResultBuffer = String -- max 2 chars long
type JsminState = (ResultBuffer,Result,StateId)
type WorkReg = (Buffer,ResultBuffer)

type Enclose = Char
type Escaped = Bool
-- -------------------------------------
type StateHandler = Buffer -> ResultBuffer -> JsminState
type ParseString = WorkReg -> Enclose -> Escaped -> JsminState

stateId2Parser :: StateId -> StateHandler
stateId2Parser x = 
	case x of
		S_String y -> s_string y
		S_Regexp -> s_regexp
		S_CommentMulti -> s_comment
		S_CommentInline -> s_comment_inline
		S_Code -> s_code

s_code :: StateHandler
s_code '/' ('/':rb) = ("",reverse rb,S_CommentInline)
s_code '*' ('/':rb)  = ("",reverse rb,S_CommentMulti)
s_code b ('/':rb) = ("",reverse (b:('/':rb)),S_Regexp)
s_code '/' rb = ("/",reverse rb,S_Code)

s_code '"' rb = ("\"",reverse rb,S_String '"')
s_code '\'' rb = ("'",reverse rb,S_String '\'')

s_code b [] = ([b],"",S_Code)
s_code b (y:rb)
	| is_wsp b && wsp_keeper y = ((b:[y]),reverse rb,S_Code)
	| is_wsp b && is_wsp y = ((y:rb),"",S_Code)
	| otherwise = ("",reverse (b:(y:rb)),S_Code)

s_string :: S_String_Type -> Buffer -> ResultBuffer -> JsminState
s_string x b rb = do
	let (rb2,r,switch) = p_simple_string x b rb
	(rb2,r,if switch then S_Code else S_String x)

p_simple_string::Char -> Buffer -> ResultBuffer -> (ResultBuffer,Result,Bool)
p_simple_string x b [] 
	| b == x = ("",[b],True)
	| otherwise = ([b],"",False)

p_simple_string x b (y:[])
	| y == '\\' = ("",y:(b:[]),False)
	| x == b = ("",y:(b:[]),True)
	| otherwise = ([b],[y],False)


s_comment_inline :: StateHandler
s_comment_inline '\n' [] = ("\n","",S_Code)
s_comment_inline b [] = ("","",S_CommentInline)

s_comment :: StateHandler
s_comment '*' rb = ("*","",S_CommentMulti)
s_comment '/' ('*':[]) = ("","",S_Code)
s_comment b rb = ("","",S_CommentMulti)

s_regexp::StateHandler
s_regexp b rb = do
	let (rb2,r,switch) = p_simple_string '/' b rb
	(rb2,r,if switch then S_Code else S_Regexp)


is_end_state :: StateId -> Bool
is_end_state sid = 
	case sid of
		S_Code -> True
		S_CommentInline -> True
		otherwise -> False

invalid_end_state_msg sid = 
	"JSMIN: Unterminated " ++ case sid of
		S_String _ -> "string"
		S_Regexp -> "regexp"
		S_CommentMulti -> "comment"
		_ -> error "ffuuuu"

invalid_end_state sid = hPutStrLn stderr (invalid_end_state_msg sid)

usage :: String -> String
usage p = "Usage: "++p++" ([<comment>] < <file to minify>) | <-t>"

comment :: [String] -> String -> String
comment (x:[]) _ = "// "++x
comment (x:xs) p = usage p
comment ([]) _ = ""

main = do
	progname <- getProgName
	args <- getArgs
	if not (null args) && (head args) == "-t"
		then run_tests
		else do
			progname <- getProgName
			putStrLn $ comment args progname
			run_jsmin stdin ("",S_Code)

run_jsmin :: Handle -> (ResultBuffer,StateId) -> IO ()
run_jsmin handle (rb,sid) = do
	is_eof <- hIsEOF handle
	if is_eof
		then
			if is_end_state sid
				then
					putStr rb
				else
					invalid_end_state sid
		else do
			input <- hGetChar handle
			let (rb2,r2,sid2) = stateId2Parser(sid) input rb
			putStr r2
			run_jsmin handle (rb2,sid2)


run_tests = putStrLn "not implemented"
