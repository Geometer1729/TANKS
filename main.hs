import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Word
import Debug.Trace
import Data.Bits (xor)
import Assembly
import System.Environment
import System.Random

type Team = Int

colormap :: Team -> Color
colormap 0 = blue
colormap 1 = red

data Tank = Tank{
	pos :: Point,
	aim :: Float,
	angle :: Float,
	team :: Team,
	memory :: Runtime,
	temp :: Int
} 

data Bullet = Bullet {
	bpos :: Point,
	bangle :: Float
}deriving(Show)

data World = World {
	tanks :: [Tank] ,
	bulets :: [Bullet],
	size :: Float,
	obs :: [(Point,Float)]
}

tau = pi*2

world = World [] [] 1000

--rendering

drawTank :: Tank -> Picture
drawTank t = Pictures $ [((translate x y) . (rotate )((-180/pi)*((angle t)-pi/2))) $ (objectToPicture (tankBody c)), translate x y (rotate ((-180/pi)*((aim t)-pi/2)) (objectToPicture (tankGun green black)))]
	where
		(x,y) = pos t
		c = colormap (team t)

drawBullet :: Bullet -> Picture
drawBullet (Bullet (x,y) _) = translate x y (circleSolid 1.5)

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
notShot bs (Tank{pos=(xt,yt)}) = not $ or [ abs (xb - xt) < 10 && abs (yb - yt) < 10 | (Bullet (xb,yb) _)  <- bs]

tankDo :: HardInst -> Tank -> [Tank] -> (Maybe Tank,Maybe Bullet) 
tankDo Sit t _  = (Just t,Nothing)
tankDo (HAim a) t _ = (Just $ t{aim=a},Nothing)
tankDo (HTurn a) t _ = (Just $ t{angle=a},Nothing)
tankDo (HScan a b) t ts = (Just $ t{memory = setRegister (setRegister (memory t) (V ra) "a") (V rb) "b"},Nothing)
	where
		(ra,rb) = scanWorld t a b ts
tankDo Die _ _ = (Nothing,Nothing)
tankDo HMove t _ = (Just t{pos = (x,y)} ,Nothing)
	where
		(tx,ty) = pos t
		ta = angle t
		(x,y) = (tx + (cos ta),ty + (sin ta))
tankDo Shoot t _ = (Just t, Just $ Bullet (pos t) (aim t))
tankDo HGPS t _ = (Just t{memory = nm,temp = temp t + 4},Nothing)
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
angleValid a b t = not $ xor (a < t) (t < b)


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
parseArgs :: [String] -> [String] -> ([String],[Bool])
parseArgs ss  [] = (ss,[])
parseArgs args (f:fs) = fmap (flag:) $ parseArgs nargs fs
	where
		nargs = filter (\a -> a /= f) args
		flag = and $ map (\a -> a /= f) args

main = do --playIO (InWindow "TANKS!" (1000,1000) (40,40)) white 30 world render handle step
	args <- getArgs
	let (nargs,[debug]) = parseArgs args ["-v"] 
	code0 <-  readFile $ head nargs
	code1 <-  readFile $ (head . tail) nargs
	let edit = labelMacro code1;
	putStrLn code1
	putStrLn edit
	let tam0 = makeTAM $ preproc code0
	let tam1 = makeTAM $ preproc code1
	print tam0
	print tam1
	g <- getStdGen
	let (g1,g2) = split g
	let xs = randomRs (0,1000) g1 :: [Float]
	let ys = randomRs (0,1000) g2 :: [Float]
	let pts = zip xs ys
	let t0ps = take 10 pts
	let t1ps = (take 10) $ (drop 10) pts
	let team0 = map (\p -> Tank {pos = p,aim = 0,angle = 0,team = 0,memory = tam0 ,temp = 0}) t0ps
	let team1 = map (\p -> Tank {pos = p,aim = 0,angle = 0,team = 1,memory = tam1 ,temp = 0}) t1ps
	let newworld = World {
			tanks = team0 ++ team1,
			bulets = [],
			size = 1000,
			obs = []
			}
	playIO (InWindow "TANKS!" (1000,1000) (40,40)) white 30 newworld render handle (step debug)

