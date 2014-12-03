data Cpx = Cpx (Int,Int) deriving (Show, Eq)

instance Num Cpx where
  (*) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a*c-b*d, a*d+b*c)
  (+) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a+c, b+d)
  (-) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a-c, b-d)
  negate (Cpx (a, b)) = Cpx (-a, -b)
  abs _ = Cpx (0, 0)
  signum _ = 0
  fromInteger a = Cpx (fromInteger a, 0)

data BinZ = BinZ [Int] deriving (Show, Eq)

getList (BinZ x) = x

instance Num BinZ where
--remember that BinZ [0,0,1,1] = 1100_z
  (+) (BinZ []) a = a
  (+) a (BinZ []) = a
  (+) (BinZ (x:xs)) (BinZ (0:ys)) = BinZ (x:getList (BinZ xs + BinZ ys))
  (+) (BinZ (0:xs)) (BinZ (x:ys)) = BinZ (x:getList (BinZ xs + BinZ ys))
  (+) (BinZ (1:1:1:xs)) (BinZ (1:1:ys)) = BinZ (0:0:getList (BinZ (0:xs) + BinZ ys))
  (+) (BinZ (1:1:xs)) (BinZ (1:1:1:ys)) = BinZ (0:0:getList (BinZ xs + BinZ (0:ys)))
  (+) (BinZ (1:xs)) (BinZ (1:ys)) = BinZ [0,0,1,1] + BinZ (0:xs) + BinZ (0:ys)
  (*) (BinZ xs) (BinZ ys) = sum $ zipWith (\a b -> if b == 1 then BinZ (a++xs) else BinZ [0]) [replicate a 0|a<-[0..]] ys
  fromInteger a = toBinZ (fromInteger a :: Cpx)
  abs a = toBinZ $ abs $ fromBinZ a

zs = Cpx (1,0):map (* Cpx (-1,1)) zs

scale s (Cpx (a, b)) = Cpx (s*a, s*b)

fromBinZ (BinZ n) = sum $ zipWith scale n zs

toBinZ (Cpx (0, 0)) = BinZ [0]
toBinZ (Cpx (a, 0))
  | a>=0 = toBinZ (Cpx (a-1,0)) + BinZ [1]
  | otherwise = toBinZ (Cpx (a+1,0)) + BinZ [1,0,1,1,1] 
toBinZ (Cpx (0, b)) = BinZ [1,1] * toBinZ (Cpx (b, 0))
toBinZ (Cpx (a, b)) = toBinZ (Cpx (a, 0)) + toBinZ (Cpx (0, b))

insigZero (0:list) = insigZero list
insigZero list = list

lengthBinZ (BinZ list) = length $ insigZero $ reverse list

cpxSpan n = map fromInteger [-n..n-1]::[Cpx]

cpxField n m = map (\x -> map (+(x*Cpx (0,1))) (cpxSpan n)) (cpxSpan m)

binary ls = sum$zipWith (*) ls (iterate (*2) 1)

zField n m = (map.map) (\x->binary$getList$toBinZ x) $ cpxField n m

main = do
  putStr$ unlines $ map unwords $(map.map) show $ zField 800 450
