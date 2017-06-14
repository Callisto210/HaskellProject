-- | Used to represent Vector, Color, Point, etc.
module Vec where

--Vec (Vec, add, sub, mulv, muld, dot, cross, norm)
newtype Vec = Vec (Double, Double, Double) deriving (Eq, Show)

-- | Add two Vec
add :: Vec -> Vec -> Vec
add (Vec(x, y, z)) (Vec(a, b, c)) = Vec(x+a, y+b, z+c)

-- | Subtract two Vec
sub :: Vec -> Vec -> Vec
sub (Vec(x, y, z)) (Vec(a, b, c)) = Vec(x-a, y-b, z-c)

-- | Multiply each element of first Vec by corresponding element in second Vec
mulv :: Vec -> Vec -> Vec
mulv (Vec(x, y, z)) (Vec(a, b, c)) = Vec(x*a, y*b, z*c)

-- | Multiplay each element in Vec by double
muld :: Vec -> Double -> Vec
muld (Vec(x, y, z)) a = Vec(x*a, y*a, z*a)

-- | Dot product
dot :: Vec -> Vec -> Double
dot (Vec(x, y, z)) (Vec(a, b, c)) = x*a + y*b + z*c

-- | Produce cross of 2 Vec
cross :: Vec -> Vec -> Vec
cross (Vec(a, b, c)) (Vec(x, y, z)) = Vec(b*z-c*y, c*x-a*z, a*y-b*x)

-- | Get Vector length
len :: Vec -> Double
len (Vec(x, y, z)) = sqrt $ x*x+y*y+z*z

-- | Normalize Vec
norm :: Vec -> Vec
norm v = v `muld` (1/len v)
