data Cpx = Cpx (Int,Int) deriving (Show, Eq)

instance Num Cpx where
  (*) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a*c-b*d, a*d+b*c)
  (+) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a+c, b+d)
  (-) (Cpx (a, b)) (Cpx (c, d)) = Cpx (a-c, b-d)
  negate (Cpx (a, b)) = Cpx (-a, -b)
  abs _ = Cpx (0, 0)
  signum _ = 0
  fromInteger a = Cpx (fromInteger a, 0)

data Bit = O|I deriving (Show, Eq, Ord)

data Binz = Sng Bit|Bit :> Binz
infixr 5 :> 

mapBinz :: (Bit -> Binz) -> Binz -> [Binz]
mapBinz f (Sng a)= [f a] 
mapBinz f (a:>as) = (f a):mapBinz f as

zipWithBinz :: (Bit -> Binz -> Binz) -> Binz -> [Binz] -> [Binz]
zipWithBinz f (Sng a) (b:bs) = [f a b]
zipWithBinz f (a:>as) [] = []
zipWithBinz f (a:>as) (b:bs) = (f a b):(zipWithBinz f as bs)

replicateBinz :: Int -> Bit -> Binz
replicateBinz 1 b = Sng b
replicateBinz n b = b:>replicateBinz (n-1) b 

instance Show Binz where
  show (Sng a) = show a
  show (a:>as) = (show a) ++ show as 

(++>) :: Binz -> Binz -> Binz
(a:>as) ++> b = a:>(as++>b)

instance Num Binz where
--remember that O:>O:>I:>Sng I = 1100_z
  (+) a (Sng O) = a
  (+) (Sng O) a = a

  (+) (Sng I) (Sng I) = O:>O:>I:>Sng I

  (+) (I:>I:>Sng I) (I:>Sng I) = Sng O
  (+) (I:>Sng I) (I:>I:>Sng I) = Sng O
  
  (+) (I:>I:>I:>xs) (I:>Sng I) = O:>O:>O:>xs
  (+) (I:>I:>Sng I) (I:>I:>ys) = O:>O:>ys

  (+) (x:>xs) (O:>ys) = x:>(xs + ys)
  (+) (I:>I:>I:>xs) (I:>I:>ys) = O:>O:>((O:>xs) + ys)
  (+) (I:>xs) (I:>ys) = O:>((O:>I:>Sng I) + xs + ys)  

  (*) xs ys = sum $ zipWithBinz
              (\a b -> if a == I then b++>xs else Sng O)
              ys
              [replicateBinz n O| n<-[0..]]
  
  --fromInteger a = toBinz (fromInteger a :: Cpx)
  --abs a = toBinz $ abs $ fromBinz a

zs :: [Cpx]
zs = Cpx (1,0):map (* Cpx (-1,1)) zs

scale s (Cpx (a, b)) = Cpx (s*a, s*b)

fromBinz n = sum $ zipWith (\a b -> scale a b) n zs
{-
toBinz (Cpx (0, 0)) = Sng O
toBinz (Cpx (a, 0))
  | a>=0 = toBinz (Cpx (a-1,0)) + Sng I
  | otherwise = toBinz (Cpx (a+1,0)) + I:>O:>I:>I:>Sng I 
toBinz (Cpx (0, b)) = (1:>Sng 1) * toBinz (Cpx (b, 0))
toBinz (Cpx (a, b)) = toBinz (Cpx (a, 0)) + toBinz (Cpx (0, b))

cpxSpan n = map fromInteger [-n..n-1]::[Cpx]

cpxField n m = map (\x -> map (+(x*Cpx (0,1))) (cpxSpan n)) (cpxSpan m)
-}
