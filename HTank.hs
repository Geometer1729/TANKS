module HTank where

import Graphics.Gloss
import Data.Word



type Team = Int

data HTank = HTank {
	pos :: Point,
	angle :: Float,
	team :: Team
} deriving(Show)
