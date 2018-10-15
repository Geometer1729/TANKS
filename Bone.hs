module Bone where

import Text.Regex
import Debug.Trace

import Assembly

type Line = (String,Int)

--Takes a list of lines starting at
--the if statement and truncates that
--list at the end of the if statement
captureIf :: [String] -> [String]
captureIf = takeWhile (\s -> head s == '*' || head s == '+' || head s == '-')

--makeIf takes a captured if as a list
--of lines of code, and returns a list
--of lines of code that is in standard
--dab
makeIf :: [Line] -> [String]
makeIf bonecode = let
		   cond    = (dropWhile ((' ')/=) $ head $ map fst bonecode, snd . head $ bonecode)
	           stars   = getHeader '*' bonecode
		   plusses = getHeader '+' bonecode
		   minusses= getHeader '-' bonecode
	          in stars ++ (createJumpCond cond ((length minusses)+(length stars)+2))
			   ++ minusses ++ ["Jmp " ++ (show $ (1+) . snd . last $ bonecode)]
			   ++ plusses

--createJumpCond takes the cond line and the number of minusses
--and returns the JmpIf part of the compiled code
createJumpCond :: Line -> Int -> [String]
createJumpCond cond l = let w = words . fst $ cond
			    compare = w !! 1
			    arg0 = head w
			    arg1 = head . tail . tail $ w
		        in case compare of
			  "==" -> ["TEQ " ++  arg0 ++ " " ++ arg1,"JmpIf " ++ (show $ (snd cond) + l)]

getHeader :: Char -> [Line] -> [String]
getHeader c ss = [tail . fst $ s | s <- ss, head (fst s) == c]

testProg :: [String]
testProg = ["Write code",
	    "if brian == sucks",
	    "* tell gabe",
	    "+ laugh at him",
	    "+ throw eraser",
	    "- become confused",
	    "- bottom text"]
