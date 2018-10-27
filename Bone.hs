module Bone where

import Text.Regex
import Debug.Trace

import Assembly

--createProgram takes Bone source and turns it into Dab
createProgram :: [String] -> [String]
createProgram source = let ifcode = findIf source
		       in if null ifcode then source else
		       let
		 	startif = length source - length ifcode
		 	ifstatement = zip (captureIf ifcode) [startif..]
			endif = startif + length ifstatement
		       in createProgram $ (take startif source) ++ (makeIf ifstatement) ++ (drop endif source)



type Line = (String,Int)

findIf :: [String] -> [String]
findIf = dropWhile (\s -> (head . words $ s) /= "if")

--Takes a list of lines starting at
--the if statement and truncates that
--list at the end of the if statement
captureIf :: [String] -> [String]
captureIf s = head s : (takeWhile (\s -> head s == '*' || head s == '+' || head s == '-') (tail s))

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
		   (op,arg0,arg1) = comp cond
		   l = (length minusses)+(length stars) + 2
	          in stars ++ case op of{-(createJumpCond cond ((length minusses)+(length stars)+2))
			   ++ minusses ++ ["Jmp " ++ (show $ (1+) . snd . last $ bonecode)]
			   ++ plusses-}
			 "==" -> ["TEQ " ++  arg0 ++ " " ++ arg1,"JmpIf " ++ (show $ (snd cond) + l)]
				++ minusses ++ ["Jmp " ++ (show $ (2+) . snd . last $ bonecode)] ++ plusses
			 "!=" -> ["TEQ " ++ arg0 ++ " " ++ arg1,"JmpIf " ++ (show $ (snd cond) + 3),
				   "Jmp " ++ (show $ (snd cond) + l + 1)]
				++ plusses ++ ["Jmp " ++ (show $ (length minusses) + (length plusses) + (snd cond) + (length stars) + 2)] ++ minusses



--comp takes a conditional expression and breaks it up
--into the specific condition and it's arguments
comp :: Line -> (String,String,String)
comp cond = let w = words . fst $ cond
	    in (w !! 1, head w, w !! 2)
{-
--createJumpCond takes the cond line and the number of minusses
--and returns the JmpIf part of the compiled code
createJumpCond :: Line -> Int -> [String]
createJumpCond cond l = let w = words . fst $ cond
			    compare = w !! 1
			    arg0 = head w
			    arg1 = head . tail . tail $ w
		        in case compare of
			  "==" -> ["TEQ " ++  arg0 ++ " " ++ arg1,"JmpIf " ++ (show $ (snd cond) + l)]
			  "!=" -> ["TEQ " ++ arg0 ++ " " ++ arg1,"JmpIf " ++ (show $ (snd cond) + 3),
				   "Jmp " ++ (show $ (snd cond) + l + 1)]
-}
getHeader :: Char -> [Line] -> [String]
getHeader c ss = [tail . fst $ s | s <- ss, head (fst s) == c]

process :: [String] -> [String]
process s = let ifst = captureIf . findIf $ s
	    in makeIf $ zip ifst [1..]

testProg :: [String]
testProg = ["Write code",
	    "Write more code",
            "Debug code",
	    "Cry",
	    "Fix code",
	    "if brian == cool",
	    "* tell gabe",
	    "+ laugh at him",
	    "+ throw eraser",
	    "- become confused",
	    "- bottom text",
	    "rest of program"]
