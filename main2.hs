import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Assembly

teamToColor :: Int -> Color
teamToColor 0 = blue
teamToColor 1 = red

type World = ([Tank],[Bullet])

data Tank = Tank {
  pos :: Point,
  aim :: Double,
  dir :: Double,
  team :: Int,
  memory :: Runtime
}

data Bullet = Bullet {
  bpos :: Point,
  bdir :: Float
}

-- rendering Code

drawOb :: (Point,Float) -> Picture
drawOb ((x,y),r) = translate x y $circleSolid r

drawTank :: Tank -> Picture
drawTank t = Pictures $ [((translate x y) . (rotate )((-180/pi)*((angle t)-pi/2))) $ (objectToPicture (tankBody c)), translate x y (rotate ((-180/pi)*((aim t)-pi/2)) (objectToPicture (tankGun green black)))]
  where
    (x,y) = pos t
    c = colormap (team t)

drawBullet :: Bullet -> Picture
drawBullet (Bullet (x,y) _) = translate x y (circleSolid 1.5)

render :: World -> IO Picture
render w = return $ translate (-size w/2) (-size w/2) $ Pictures $ (map drawTank (tanks w)) ++ (map drawBullet (bulets w)) ++ (map drawOb (obs w))

objectToPicture :: Object -> Picture
objectToPicture o = Pictures $ map (\ (xs,c) -> color c $ drawShape xs) o

drawShape :: Shape -> Picture
drawShape (Pol p) = Polygon p
drawShape (Circ ((x,y),r)) = translate x y (circleSolid r)

type Object = [Part]
type Part = (Shape,Color)
data Shape = Pol Polygon | Circ Circle deriving Show
type Polygon = [Point]
type Circle = (Point,Float)

tankBody :: Color -> Object
tankBody c = obShift (-10,-10) [(Pol [(0,0),(0,20),(4,20),(4,0)],black),(Pol [(4,2),(4,18),(16,18),(16,2)],c),(Pol [(16,0),(16,20),(20,20),(20,0)],black)]

tankGun :: Color -> Color -> Object
tankGun c0 c1= obShift (-10,-10) [(Circ ((10,10),4),c0),(Pol [(8,8),(8,20),(12,20),(12,8)],c1)]

mapPts :: (Point -> Point) -> Object -> Object
mapPts _ [] = []
mapPts f (((Pol pts),c):o)  = (Pol (map f pts),c) : mapPts f o
mapPts f ((Circ(pt,r),c):o) = (Circ (f pt,r),c) : mapPts f o

obShift :: Point -> Object -> Object
obShift p = mapPts (ptShift p)

ptShift :: Point->Point->Point
ptShift (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- steping the world

move :: Point -> Tank -> Tank
move (dx,dy) t@Tank{pos=(x,y)} = t{pos=(x+dx,y+dy)}


main = return ()
