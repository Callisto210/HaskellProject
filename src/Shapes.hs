module Shapes where

import Vec
import Material
import Ray
import Debug.Trace

data Shape = Sphere
                {
                  radius :: Double
                , position :: Vec
                , emission :: Vec
                , color :: Vec
                , material :: Material
                } deriving (Show, Eq)
                

intersect :: Shape -> Ray -> Maybe (Shape, Double) --returns Just distance or Nothing if no hit
intersect sphere ray =
    let
        op = (position sphere) `sub` (origin ray)
        b = op `dot` (direction ray)
        det = (b^2) - (op `dot` op) + (radius sphere)^2
    in
        if b < 0.0
            then Nothing
            else let
                    sqrtdet = sqrt(det)
                    s1 = b-sqrtdet
                    s2 = b+sqrtdet
                 in
                    if s2 > 0.0
                    then if s1 > 0.0
                         then Just (sphere,s1)
                         else Just (sphere,s2)
                    else Nothing
        
getSurfaceNormal :: Shape -> Vec -> Ray -> Vec
getSurfaceNormal sphere x ray =
    let
        n = norm (x `sub` position sphere)
    in
        if (n `dot` direction ray) < 0.0
        then n
        else n `muld` (-1.0)
        
        
getLightSources :: [Shape] -> [Shape]
getLightSources shapes =
    filter filter_fun shapes
        where 
            filter_fun = \shape ->   let
                                        Vec(r, g, b) = emission shape
                                     in
                                        if r > 0.0 || g > 0.0 || b > 0.0
                                            then True
                                            else False
        
