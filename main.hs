import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Word
import Debug.Trace

data Instruction = Sit | Shoot | Scan Float Float | Aim Float | Move

type Team = Int

colormap :: Team -> Color
colormap 0 = blue
colormap 1 = red 

data Tank = Tank{
	pos :: Point,
	angle :: Float,
	team :: Team,
	memory :: Word64
}deriving(Show)

data Bullet = Bullet {
	bpos :: Point,
	bangle :: Float
}deriving(Show)

data World = World {
	tanks :: [Tank] ,
	bulets :: [Bullet],
	size :: Float
}

tau = pi*2

test1 :: Tank
test1 = Tank (10,10) 0 0 0

test2 :: Tank
test2 = Tank (490,490) pi 1 0

world = World [test1,test2] [] 1000

render :: World -> IO Picture
render w = return $ translate (-size w/2) (-size w/2) $ Pictures $ (map drawTank (tanks w)) ++ (map drawBullet (bulets w))

stepBullet :: Bullet -> Bullet
stepBullet (Bullet (x,y) r) = Bullet (x+5*(cos r),y+5*(sin r)) r

stepBulleter :: Bullet -> Bullet
stepBulleter (Bullet (x,y) r) = Bullet (x+30*(cos r),y+30*(sin r)) r

drawTank :: Tank -> Picture
drawTank t = Pictures $ [translate x y (objectToPicture (tankBody c)), translate x y (rotate ((-180/pi)*((angle t)-pi/2)) (objectToPicture (tankGun green black)))]
	where
		(x,y) = pos t
		c = colormap (team t)
drawBullet :: Bullet -> Picture
drawBullet (Bullet (x,y) _) = translate x y (circle 1)


handle :: Event -> World -> IO World
handle _ w = return w 	

step :: Float -> World -> IO World
step _ w = trace (show (ts,nts,(bulets w),bs)) return w{tanks = sts, bulets = filter bulletInMap $ map stepBullet $ nbs} 
	where
		sts = filter (notShot nbs) nts
		nbs = (bulets w) ++ (map stepBulleter bs)
		ts = tanks w :: [Tank]
		is = zipWith (\(i,m) t -> (t{memory=m},i)) (map (ai.memory) ts) ts :: [(Tank,Instruction)]
		(nts,bs) = handleTanks is	

bulletInMap :: Bullet -> Bool
bulletInMap (Bullet (x,y) _) = (abs x < 1000) &&  (abs y < 1000)

notShot :: [Bullet] -> Tank -> Bool
notShot bs (Tank (xt,yt) _ _ _) = not $ or [ abs (xb - xt) < 10 && abs (yb - yt) < 10 | (Bullet (xb,yb) _)  <- bs] 


handleTanks :: [(Tank,Instruction)]-> ([Tank],[Bullet])
handleTanks [] = ([],[])
handleTanks ((t,i):its) = tankHelper i (t,its,[],[])

tankHelper :: Instruction -> (Tank,[(Tank,Instruction)],[Tank],[Bullet]) -> ([Tank],[Bullet])
tankHelper i (t,[],ts,bs) = (nt:ts,nbs)
	where
		(nt,_,nbs) = tankDo i (t,[],ts,bs)
tankHelper i (t,its,ts,bs) = tankHelper nni (nnt,tail its,nt:ts,nbs) 
	where
		(nt,nits,nbs) = tankDo i (t,its,ts,bs)
		(nnt,nni) = head its
		

tankDo :: Instruction -> (Tank,[(Tank,Instruction)],[Tank],[Bullet]) -> (Tank,[(Tank,Instruction)],[Bullet])
tankDo Sit (t,its,_,bs) = (t,its,bs)
tankDo (Aim a) (t,its,_,bs) = (t{angle=a},its,bs)
tankDo (Scan a b) (t,its,ts,bs) = (t{memory = readScan (scanWorld t a b ts) (memory t) },its,bs)
tankDo Move (t,its,_,bs) = (t{pos = (x,y)} ,its,bs)
	where
		(tx,ty) = pos t
		ta = angle t
		(x,y) = (tx + (cos ta),ty + (sin ta)) 
tankDo Shoot (t,its,ts,bs) = (t,its, Bullet (pos t) (angle t):bs)

shoots :: Tank -> Tank -> Bool
shoots t1 t2 = abs ((angle t1) - (getAngle t1 t2)) < (pi / 512)

getAngle :: Tank -> Tank -> Float
getAngle t1 t2 = ca
	where	
		(x1,y1) = pos t1
		(x2,y2) = pos t2
		ca = (if y2 > y1 then 0 else pi)  + atan ((y2-y1)/(x2-x1)) 

scanWorld :: Tank -> Float -> Float -> [Tank] -> Bool
scanWorld t1 a b ts = or [ (a < (getAngle t1 t2)) && ((getAngle t1 t2) < b) | t2 <- ts  ]

ai :: Word64 -> (Instruction,Word64)
ai n = if n < 4096 then ( if mod n 2 == 0 then (Aim ( (fromIntegral) n * (tau/4096)) ,n+1) else (Shoot,n+1) ) else (Aim 0,0)

readScan :: Bool -> Word64 -> Word64
readScan s m =  2 * (div m 2) + if s then 1 else m

main = playIO (InWindow "TANKS!" (1000,1000) (40,40)) white 30 world render handle step 

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
tankBody c = obShift (-10,-10) [(Pol [(0,0),(0,20),(4,20),(4,0)],black),(Pol [(4,2),(4,18),(16,18),(16,2)],c),(Pol [(16,0),(16,20),(20,20),(20,0)],black)]

tankGun :: Color -> Color -> Object
tankGun c0 c1= obShift (-10,-10) [(Circ ((10,10),4),c0),(Pol [(8,8),(8,20),(12,20),(12,8)],c1)]

mapPts::(Point -> Point) -> Object -> Object
mapPts _ [] = []
mapPts f (((Pol pts),c):o)  = (Pol (map f pts),c) : mapPts f o
mapPts f ((Circ(pt,r),c):o) = (Circ (f pt,r),c) : mapPts f o

obShift::Point -> Object -> Object
obShift p = mapPts (ptShift p)

ptShift::Point->Point->Point
ptShift (x1,y1) (x2,y2) = (x1+x2,y1+y2)


