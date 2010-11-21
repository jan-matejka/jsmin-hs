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

until' :: String -> (String -> String) ->  WorkReg -> (WorkReg,Bool)
until' xs f (a,b)
	| length xs > length a = ((a,b),False)
	| take (length xs) a == xs = ((drop (length xs) a, b++(f xs)), True)
	| True = until' xs f (tail a, b++(f [head a]))


type Buffer = String
type Result = String
type StateId = String
type WorkReg = (Buffer,Result)
type JsminState = (WorkReg, StateId)
type Enclose = Char
type Escaped = Bool
-- -------------------------------------
type StateHandler = WorkReg -> JsminState
type ParseString = WorkReg -> Enclose -> Escaped -> JsminState


stateId2Parser :: StateId -> StateHandler
stateId2Parser x = 
	case x of
		"string" -> s_string
		"regexp" -> s_regexp
		"comment" -> s_comment
		"comment_inline" -> s_comment_inline
		"code" -> s_code
		_ -> error "bollox"

s_code :: StateHandler
s_code ('/':('/':bs),r) = (("//"++bs,r),"comment_inline")
s_code ('/':('*':bs),r) = (("/*"++bs,r),"comment")
s_code ('/':bs,r) = (("/"++bs,r),"regexp")
s_code ('"':bs,r) = ((['"']++bs,r),"string")
s_code ('\'':bs,r) = ((['\'']++bs,r),"string")
s_code (b:bs,r) = ((bs,r++[b]),"code")
s_code ([],r) = (([],r),"code")


s_comment_inline :: StateHandler
s_comment_inline wr = do
	let (wr1,_) = until' "\n" ommit wr
	(wr1,"code")

s_comment :: StateHandler
s_comment (b:[],r) = jsmin_error "comment"
s_comment wr = do
	let (wr1,f) = until' "*/" ommit wr
	if f then (wr1,"code")
		else jsmin_error "comment"


s_string::StateHandler
s_string (bs,r) = p_string (bs,r) (head bs) True

s_regexp::StateHandler 
s_regexp (bs,r) = p_regexp (bs,r) True

p_regexp :: WorkReg -> Escaped -> JsminState
p_regexp ([],r) ed = (([],r),"regexp")
p_regexp ('\\':bs,r) False = p_regexp (bs,r++"\\") True
p_regexp ('\\':bs,r) True = p_regexp (bs,r++"\\") False
p_regexp (b:bs,r) ed
	| (b == '/') = if (ed == False)
		then ((bs,r++"/"),"code")
		else p_regexp (bs,r++[b]) False
	| True = p_regexp (bs,r++[b]) False

p_string :: ParseString
p_string ([],r) en ed = (([],r),"string")
p_string ('\\':bs,r) en False = p_string (bs,r++"\\") en True
p_string ('\\':bs,r) en True = p_string (bs,r++"\\") en False
p_string (b:bs,r) en ed
	| (b == en) = if (ed == False)
		then ((bs,r++[en]),"code")
		else p_string (bs,r++[b]) en False
	| True = p_string (bs,r++[b]) en False


is_end_state :: String -> Bool
is_end_state s = s `elem` ["code"]

jsmin :: State JsminState JsminState
jsmin = do
	((b,r),sid) <- get
	let (reg,sid) = stateId2Parser(sid) (b,r)
	put (reg,sid)
	if (null (fst reg)) 
		then if is_end_state(sid)
			then return (reg,sid)
			else jsmin_error sid
		else jsmin

--jsmin_error :: String -> 
jsmin_error s = error ("Error: Unterminated "++s)

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
		else run_jsmin args

run_jsmin args = do
	progname <- getProgName
	putStrLn $ comment args progname
	input <- getContents
	let ((b,r),sid) = evalState (jsmin) ((input,""),"code")
	putStr r


run_tests = do
	test "comment" "/*a*/b" (("b",""),"code")
	test "comment" "/*/b" (("",""),"comment")
	test "comment_inline" "//a" (("",""),"code")
	test "comment_inline" "//a\nb" (("b","\n"),"code")
	test_e "comment" "/*!" (jsmin_error "comment")
	test "string" "\"a\"b" (("b","\"a\""),"code")
	test "string" "\"a\\\"b\"c" (("c","\"a\\\"b\""),"code")
	test "string" "''a" (("a","''"),"code")
	test "string" "'a'b" (("b","'a'"),"code")
	test "string" "'a\\'b'c" (("c","'a\\'b'"),"code")
	test "regexp" "/a/b" (("b","/a/"),"code")
	test "regexp" "/a\\/b/" (("","/a\\/b/"),"code")
	test "code" "//a" (("//a",""),"comment_inline")
	test "code" "'a" (("'a",""),"string")
	test "code" "\"a" (("\"a",""),"string")
	test "code" "/*a" (("/*a",""),"comment")
	test "code" "/a" (("/a",""),"regexp")

test :: String -> String -> JsminState -> IO ()
test state input wr = do
	let ((b,r),s) = (stateId2Parser state) (input,"")
	putStrLn $ "test "++state++" "++input
	putStrLn $"result: "++r ++ " ;buffer: "++b++ " ;state: "++s
	putStrLn $ "test result: "++(if ((b,r),s) == wr then "WIN" else "FAIL")
	putStrLn ""

test_e state input e = do
		let ((b,r),s) = (stateId2Parser state) (input,"")

		putStrLn ""

