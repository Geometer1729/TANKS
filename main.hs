import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Team = Int

colormap :: Team -> Color
colormap 0 = blue
colormap 1 = red

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
test1 = Tank (10,10) 0 0

test2 :: Tank
test2 = Tank (490,490) pi 1

world = World [test1,test2] 500

render :: World -> IO Picture
render w = return $ translate (-size w/2) (-size w/2) $ Pictures $ map drawTank (tanks w)

drawTank :: Tank -> Picture
drawTank t = Pictures $ [translate x y (objectToPicture (tankBody c)), translate x y (rotate (angle t) (objectToPicture (tankGun green black)))]
	where
		(x,y) = pos t
		c = colormap (script t)


handle :: Event -> World -> IO World
handle _ w = return w -- handles no events

step :: Float -> World -> IO World
step _ = return

main = playIO (InWindow "TANKS!" (1000,1000) (200,200)) white 30 world render handle step 


objectToPicture :: Object -> Picture
objectToPicture o = Pictures $ map (\ (xs,c) -> color c $ drawShape xs) o

drawShape:: Shape -> Picture
drawShape (Pol p) = Polygon p
drawShape (Circ ((x,y),r)) = translate x y (circleSolid r)

type Object = [Part]
type Part = (Shape,Color)
data Shape = Pol Polygon | Circ Circle deriving Show
type Polygon = [Point]
type Circle = (Point,Float)

tankBody :: Color -> Object
tankBody c = [(Pol [(0,0),(0,20),(4,20),(4,0)],black),(Pol [(4,2),(4,18),(16,18),(16,2)],c),(Pol [(16,0),(16,20),(20,20),(20,0)],black)]

tankGun :: Color -> Color -> Object
tankGun c0 c1= [(Circ ((10,10),4),c0),(Pol [(8,8),(8,20),(12,20),(12,8)],c1)]

