module HTank where

import Graphics.Gloss
import Data.Word

data Instruction = Sit | Shoot | Scan Float Float | Aim Float | Move

type Team = Int

data HTank = HTank {
	pos :: Point,
	angle :: Float,
	team :: Team
} deriving(Show)




