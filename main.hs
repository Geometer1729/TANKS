import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Word
import Debug.Trace
import Data.Bits (xor)
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

--rendering

drawTank :: Tank -> Picture
drawTank t = Pictures $ [translate x y (objectToPicture (tankBody c)), translate x y (rotate ((-180/pi)*((angle t)-pi/2)) (objectToPicture (tankGun green black)))]
	where
		(x,y) = pos t
		c = colormap (team t)
drawBullet :: Bullet -> Picture
drawBullet (Bullet (x,y) _) = translate x y (circle 1)

render :: World -> IO Picture
render w = return $ translate (-size w/2) (-size w/2) $ Pictures $ (map drawTank (tanks w)) ++ (map drawBullet (bulets w))

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

--steping world

step :: Bool -> Float -> World -> IO World
step debug _ w = do 
	rs <- mapM (\t -> let mt = memory t in 
		if debug && team t == 1 then runDebug mt else return (run mt) ) (tanks w)
	let is = map (\((nm,i),t) -> (t{memory=nm},i)) (zip rs (tanks w))
	let ts = tanks w	
	let wr = map (\(t,i) -> tankDo i t ts) is :: [(Maybe Tank,Maybe Bullet)]
	let nts = justice . (map fst) $ wr :: [Tank]
	let bs = justice . (map snd) $ wr ::[Bullet]
	let nbs = (bulets w) ++ (map stepBulleter bs)
	let sts = filter (notShot nbs) nts
	return w{tanks = sts, bulets = filter bulletInMap $ map stepBullet $ nbs}

justice :: [Maybe a] -> [a]
justice ((Just x):xs) = x:(justice xs)
justice ((Nothing):xs) = justice xs
justice [] = []

stepBullet :: Bullet -> Bullet
stepBullet (Bullet (x,y) r) = Bullet (x+5*(cos r),y+5*(sin r)) r

stepBulleter :: Bullet -> Bullet
stepBulleter (Bullet (x,y) r) = Bullet (x+30*(cos r),y+30*(sin r)) r

bulletInMap :: Bullet -> Bool
bulletInMap (Bullet (x,y) _) = (abs x < 1000) &&  (abs y < 1000)

notShot :: [Bullet] -> Tank -> Bool
notShot bs (Tank (xt,yt) _ _ _) = not $ or [ abs (xb - xt) < 10 && abs (yb - yt) < 10 | (Bullet (xb,yb) _)  <- bs]
notShot _ DeadTank = False

tankDo :: HardInst -> Tank -> [Tank] -> (Maybe Tank,Maybe Bullet) 
tankDo Sit t _  = (Just t,Nothing)
tankDo (HAim a) t _ = (Just $ t{angle=a},Nothing)
tankDo (HScan a b) t ts = (Just $ t{memory = setRegister (setRegister (memory t) (V ra) "a") (V rb) "b"},Nothing)
	where
		(ra,rb) = scanWorld t a b ts
tankDo Die _ _ = (Nothing,Nothing)
tankDo HMove t _ = (Just t{pos = (x,y)} ,Nothing)
	where
		(tx,ty) = pos t
		ta = angle t
		(x,y) = (tx + (cos ta),ty + (sin ta))
tankDo Shoot t _ = (Just t, Just $ Bullet (pos t) (angle t))
tankDo HGPS t _ = (Just t{memory = nm},Nothing)
	where
		(tx,ty) =  pos t
		nm = setRegister (setRegister (memory t) (V $ round tx) "a") (V $ round ty) "b"
tankDo HGyro t _ = (Just t{memory = nm},Nothing)
	where
		ta = angle t
		nm = setRegister (memory t) (V $ round ta) "a"

scanWorld :: Tank -> Float -> Float -> [Tank] -> (Int,Int)
scanWorld t1 a b ts = (length $ filter (\t -> team t == team t1) sTs, length $ filter (\t -> team t /= team t1) sTs)
	where
		sTs =  [ t2 | t2 <- ts , angleValid a b (getAngle t1 t2)  ]

angleValid :: Float -> Float -> Float -> Bool
angleValid a b t = xor (a < t) (t < b)


getAngle :: Tank -> Tank -> Float
getAngle t1 t2 = ca
	where
		(x1,y1) = pos t1
		(x2,y2) = pos t2
		ca = (if y2 > y1 then 0 else pi)  + atan ((y2-y1)/(x2-x1))

--if we ever add controll fuck with this part
handle :: Event -> World -> IO World
handle _ w = return w

--main
main = do --playIO (InWindow "TANKS!" (1000,1000) (40,40)) white 30 world render handle step
	args <- getArgs
	let debug = (head args) == "-v"
	let nargs = (if debug then tail else id ) args
	code1 <-  readFile $ head nargs
	code2 <-  readFile $ (head . tail) nargs
	let edit = labelMacro code2;
	putStrLn code2
	putStrLn edit
	let tam1 = makeTAM $ preproc code1
	let tam2 = makeTAM $ preproc code2
	print tam1
	print tam2
	let tank1 = Tank {
			pos = (500,300),
			angle = 1.57,
			team = 0,
			memory = tam1
		   }
	let tank2 = Tank {
		pos = (600,700),
		angle = (3/4)*tau,
		team = 1,
		memory = tam2
	}
	let newworld = World {
			tanks = [tank1,tank2],
			bulets = [],
			size = 1000
			}
	let newtam = exec tam1
	--print newtam
	playIO (InWindow "TANKS!" (1000,1000) (40,40)) white 30 newworld render handle (step debug)

