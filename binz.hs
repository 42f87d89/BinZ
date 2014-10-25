data Cpx = Cpx (Int,Int) deriving (Show, Eq)

instance Num Cpx where
  (*) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a*c-b*d, a*d+b*c)
  (+) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a+c, b+d)
  (-) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a-c, b-d)
  negate (Cpx (a, b)) = Cpx (-a, -b)
  abs _ = 0
  signum _ = 0
  fromInteger a = Cpx (fromInteger a, 0)


data BinZ = BinZ [Int] deriving (Show, Eq)

getList (BinZ x) = x

instance Num BinZ where
  (+) (BinZ []) a = a
  (+) a (BinZ []) = a
  (+) (BinZ (x:xs)) (BinZ (0:ys)) = BinZ (x:getList (BinZ xs + BinZ ys))
  (+) (BinZ (0:xs)) (BinZ (x:ys)) = BinZ (x:getList (BinZ xs + BinZ ys))
  (+) (BinZ (1:1:1:xs)) (BinZ (1:1:ys)) = BinZ (0:0:0:xs) + BinZ (0:0:ys)
  (+) (BinZ (1:1:xs)) (BinZ (1:1:1:ys)) = BinZ (0:0:xs) + BinZ (0:0:0:ys)
  (+) (BinZ (1:xs)) (BinZ (1:ys)) = BinZ [0,0,1,1] + BinZ (0:xs) + BinZ (0:ys)
  (*) a b = 
  --(-) (BinZ a) (BinZ b) = 
  --negate = 
  abs _ = BinZ [0]
  signum _ = BinZ [0]
  fromInteger a = BinZ [0]

zs = Cpx (1,0):map (* Cpx (-1,1)) zs

scale s (Cpx (a, b)) = Cpx (s*a, s*b)

--remember that BinZ [0,0,1,1] = 1100_z

fromBinZ (BinZ n) = sum $ zipWith scale n zs

toBinZ (Cpx (0, 0)) = BinZ [0]
toBinZ (Cpx (a, 0)) = toBinZ (Cpx (a-1,0)) + BinZ [1]
toBinZ (Cpx (0, b)) = BinZ [1,1] * toBinZ (Cpx (b, 0))
toBinZ (Cpx (a, b)) = toBinZ (Cpx (a, 0)) + toBinZ (Cpx (0, b))
