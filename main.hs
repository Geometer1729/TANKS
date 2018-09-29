import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Team = Int

data Tank = Tank{
	pos :: Point,
	angle :: Float,
	script :: Team
}

data World = World {
	tanks :: [Tank] ,
	size :: Float
}

test1 :: Tank
test1 = Tank (10,10) 0 1

test2 :: Tank
test2 = Tank (490,490) pi 2

world = World [test1,test2] 500

render :: World -> IO Picture
render w = return $ translate (-size w/2) (-size w/2) $ Pictures $ map drawTank (tanks w)

drawTank :: Tank -> Picture
drawTank t = translate x y (circleSolid 10)
	where
		(x,y) = pos t


handle :: Event -> World -> IO World
handle _ w = return w -- handles no events

step :: Float -> World -> IO World
step _ = return

main = playIO (InWindow "TANKS!" (500,500) (200,200)) white 30 world render handle step 



