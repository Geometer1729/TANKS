import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Word
import Debug.Trace
import Assembly
import System.Environment

type Team = Int

colormap :: Team -> Color
colormap 0 = blue
colormap 1 = red 

data Tank = Tank{
	pos :: Point,
	angle :: Float,
	team :: Team,
	memory :: Runtime
} | DeadTank

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

world = World [] [] 1000

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
step _ w = return w{tanks = sts, bulets = filter bulletInMap $ map stepBullet $ nbs} 
	where
		sts = filter (notShot nbs) nts
		nbs = (bulets w) ++ (map stepBulleter bs)
		ts = tanks w :: [Tank]
		is = zipWith (\(m,i) t -> (t{memory=m},i)) (map (run .memory) ts) ts :: [(Tank,HardInst)]
		(nts,bs) = handleTanks is	

bulletInMap :: Bullet -> Bool
bulletInMap (Bullet (x,y) _) = (abs x < 1000) &&  (abs y < 1000)

notShot :: [Bullet] -> Tank -> Bool
notShot bs (Tank (xt,yt) _ _ _) = not $ or [ abs (xb - xt) < 10 && abs (yb - yt) < 10 | (Bullet (xb,yb) _)  <- bs] 


handleTanks :: [(Tank,HardInst)]-> ([Tank],[Bullet])
handleTanks [] = ([],[])
handleTanks ((t,i):its) = tankHelper i (t,its,[],[])

tankHelper :: HardInst -> (Tank,[(Tank,HardInst)],[Tank],[Bullet]) -> ([Tank],[Bullet])
tankHelper i (t,[],ts,bs) = (if nt == DeadTank then ts else nt:ts,nbs)
	where
		(nt,_,nbs) = tankDo i (t,[],ts,bs)
tankHelper i (t,its,ts,bs) = tankHelper nni (nnt,tail its,if nt == DeadTank then ts else nt:ts,nbs) 
	where
		(nt,nits,nbs) = tankDo i (t,its,ts,bs)
		(nnt,nni) = head its
		

tankDo :: HardInst -> (Tank,[(Tank,HardInst)],[Tank],[Bullet]) -> (Tank,[(Tank,HardInst)],[Bullet])
tankDo Sit (t,its,_,bs) = (t,its,bs)
tankDo (HAim a) (t,its,_,bs) = (t{angle=a},its,bs)
tankDo (HScan a b) (t,its,ts,bs) = (t{memory = setRegister (setRegister (memory t) (V ra) "a") (V rb) "b"},its,bs)
	where
		(ra,rb) = scanWorld t a b ts
tankDo Die (t,its,_,bs) = (DeadTank,its,ts,bs)	
tankDo HMove (t,its,_,bs) = (t{pos = (x,y)} ,its,bs)
	where
		(tx,ty) = pos t
		ta = angle t
		(x,y) = (tx + (cos ta),ty + (sin ta))
tankDo Shoot (t,its,ts,bs) = (t,its, Bullet (pos t) (angle t):bs)
tankDo HGPS (t,its,ts,bs) = (t{memory = nm},its,bs)
	where
		(tx,ty) =  pos t
		nm = setRegister (setRegister (memory t) (V $ round tx) "a") (V $ round ty) "b"
tankDo HGyro (t,its,ts,bs) = (t{memory = nm},its,bs) 
	where 
		ta = angle t
		nm = setRegister (memory t) (V $ round ta) "a"

shoots :: Tank -> Tank -> Bool
shoots t1 t2 = abs ((angle t1) - (getAngle t1 t2)) < (pi / 512)

getAngle :: Tank -> Tank -> Float
getAngle t1 t2 = ca
	where	
		(x1,y1) = pos t1
		(x2,y2) = pos t2
		ca = (if y2 > y1 then 0 else pi)  + atan ((y2-y1)/(x2-x1)) 

scanWorld :: Tank -> Float -> Float -> [Tank] -> (Int,Int)
scanWorld t1 a b ts = (length $ filter (\t -> team t == team t1) sTs, length $ filter (\t -> team t /= team t1) sTs)
	where
		sTs =  [ t2 | t2 <- ts , (a < (getAngle t1 t2)) && ((getAngle t1 t2) < b) ]

main = do --playIO (InWindow "TANKS!" (1000,1000) (40,40)) white 30 world render handle step 
	args <- getArgs
	code1 <- readFile $ head args
	code2 <- readFile $ (head . tail) args
	let tam1 = makeTAM code1
	let tam2 = makeTAM code2
	print tam1
	print tam2
	let tank1 = Tank {
			pos = (500,300),
			angle = 1.57,
			team = 0,
			memory = tam1
		   }
	let tank2 = Tank {
		pos = (500,700),
		angle = (3/4)*tau,
		team = 1,
		memory = tam2
	}
	let newworld = World {
			tanks = [tank1,tank2],
			bulets = [],
			size = 1000
			}
	playIO (InWindow "TANKS!" (1000,1000) (40,40)) white 30 newworld render handle step 

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


