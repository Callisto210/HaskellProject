module Vec where

--Vec (Vec, add, sub, mulv, muld, dot, cross, norm)
newtype Vec = Vec (Double, Double, Double) deriving (Eq, Show)

add :: Vec -> Vec -> Vec
add (Vec(x, y, z)) (Vec(a, b, c)) = Vec(x+a, y+b, z+c)

sub :: Vec -> Vec -> Vec
sub (Vec(x, y, z)) (Vec(a, b, c)) = Vec(x-a, y-b, z-c)

mulv :: Vec -> Vec -> Vec
mulv (Vec(x, y, z)) (Vec(a, b, c)) = Vec(x*a, y*b, z*c)

muld :: Vec -> Double -> Vec
muld (Vec(x, y, z)) a = Vec(x*a, y*a, z*a)

dot :: Vec -> Vec -> Double
dot (Vec(x, y, z)) (Vec(a, b, c)) = x*a + y*b + z*c

cross :: Vec -> Vec -> Vec
cross (Vec(a, b, c)) (Vec(x, y, z)) = Vec(b*z-c*y, c*x-a*z, a*y-b*x)

len :: Vec -> Double
len (Vec(x, y, z)) = sqrt $ x*x+y*y+z*z

norm :: Vec -> Vec
norm v = v `muld` (1/len v)
