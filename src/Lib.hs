module Lib where
import Vec
import Shapes
import Ray
import Material
import Data.Maybe
import Rand
import System.Random.MWC
import Data.Array
import Debug.Trace

clamp x | x < 0.0 = 0.0
        | x > 1.0 = 1.0
        | otherwise = x

-- | Return shapes hit by ray
hit :: [Shape] -> Ray ->  [(Shape, Double)]
hit shapes ray = 
    case mapMaybe (\s -> intersect s ray) shapes of
        [] -> []
        l -> [foldl1 (\acc@(s1, d1) p@(s2, d2) -> if d1 < d2 then acc else p) l]


-- | Return color of ray interacted with scene
radiance :: [Shape] -> Ray -> [Double] -> Int -> Double -> Vec  --return color
radiance shapes ray rands depth e =
    case (hit shapes ray) of
        [] -> Vec(0.0, 0.0, 0.0)
        [(obj, d)] -> let 
                        x = (origin ray) `add` ((direction ray) `muld` d) -- ray intersect point
                        col@(Vec(r, g, b)) = color obj
                        n = norm (x `sub` position obj)
                        nl = getSurfaceNormal obj x ray --need to compute if ray is entering glass, or leaving
                        p = maximum [r, g, b] --need compute Russian roulette
                        --first part of russian roulette
                        colp = if depth >= 5 || p == 0.0
                                then if (head rands) < p
                                        then col `muld` (1/p)
                                        else col
                                else col
                      in
                        --russian roulette second part
                        if (depth >= 5 || p == 0.0) && (head rands) >= p
                            then (emission obj) `muld` e
                            else case material obj of
                                    DIFF -> radianceDIFF shapes ray rands depth e n nl x obj colp
                                    SPEC -> radianceSPEC shapes ray rands depth e n nl x obj colp
                                    REFR -> radianceREFR shapes ray rands depth e n nl x obj colp

-- | Return color if ray hit diffuse shape                            
radianceDIFF shapes ray rands depth e n nl x obj colp =                       
    let 
        r1 = 2*pi*(head $ drop 1 $ rands)
        r2 = (head $ drop 2 $ rands)
        r2s = sqrt(r2)
        w@(Vec(xx, _, _)) = nl
        u = norm $ w `cross` if (abs xx) > 0.1
                               then Vec(0.0, 1.0, 0.0)
                               else Vec(1.0, 0.0, 0.0)
        v = w `cross` u
        d = norm ((u `muld` ((cos r1)*r2s)) `add` (v `muld` ((sin r1)*r2s)) `add` (w `muld` (sqrt (1-r1)) ))
        light = \acc s -> let
                            sw@(Vec(xxx, _, _)) = (position s) `sub` x
                            su = norm $ w `cross` if (abs xx) > 0.1
                                                    then Vec(0.0, 1.0, 0.0)
                                                    else Vec(1.0, 0.0, 0.0)
                            sv = sw `cross` su
                            h = (x `sub` (position s))
                            cos_a_max = sqrt(1- ((radius s)^2)/(h `dot` h))
                            eps1 = (head $ drop 3 $ rands)
                            eps2 = (head $ drop 4 $ rands)
                            cos_a = 1-eps1+(eps1*cos_a_max)
                            sin_a = sqrt(1-cos_a^2)
                            phi = 2*pi*eps2
                            l = norm ((su `muld` ((cos phi)*sin_a)) `add` (sv `muld` ((sin phi)*sin_a)) `add` (sw `muld` cos_a))
                            sph = hit shapes (Ray x l)
                        in
                            case sph of
                                [(sp, _)] ->   let
                                                    omega = 2*pi*(1-cos_a_max)
                                               in
                                                    if sp == s 
                                                        then acc `add` ( ( colp `mulv` ( (emission s) `muld` ( (l `dot` nl) * omega))) `muld` (1/pi))
                                                        else acc
                                _ -> acc
        em = foldl (light) (Vec(0.0, 0.0, 0.0)) (getLightSources shapes)                    
    in
        ((emission obj) `muld` e) `add` em `add` (colp `mulv` (radiance shapes (Ray x d) (drop 5 rands) (depth+1) 0))

-- | Return color if ray hit specular (mirror) shape    
radianceSPEC shapes ray rands depth e n nl x obj colp =
    (emission obj) `add` (colp `mulv` (radiance shapes (Ray x ((direction ray) `sub` (n `muld` ndrd `muld` 2))) (drop 5 rands) (depth+1) 1))
        where
            ndrd = n `dot` (direction ray)

-- | Return color if ray hit glass shape                            
radianceREFR shapes ray rands depth e n nl x obj colp =
    let
        ndrd = n `dot` (direction ray)
        refl = Ray x ((direction ray) `sub` (n `muld` ndrd `muld` 2))
        into = (n `dot` nl)>0
        nc = 1.0
        nt = 1.5
        nnt = if into
                then nc/nt
                else nt/nc
        ddn = (direction ray) `dot` nl
        cos2t = 1-((nnt^2)*(1-(ddn^2)))
    in
        if cos2t < 0 --total internal reflection
            then (emission obj) `add` (colp `mulv` (radiance shapes refl (drop 5 rands) (depth+1) 1))
            else let
                    yon = if into
                            then 1.0
                            else (-1.0)
                    tdir = norm (((direction ray) `muld` nnt) `sub` (n `muld` yon `muld` (ddn*nnt + sqrt(cos2t))) )
                    a = nt-nc
                    b = nt+nc
                    c = if into
                            then (ddn * (-1.0))
                            else tdir `dot` n
                    r0 = (a^2)/(b^2)
                    re = r0 + (1-r0)*(c^5)
                    tr = 1-re
                    p = 0.25 + 0.5*re
                    rp = re/p
                    tp = tr/(1-p)
                    ran = head $ drop 6 $ rands
                    next_rands = drop 7 $ rands
                 in
                    (emission obj) `add` (colp `mulv` if depth > 2
                                                    then if ran < p
                                                            then ( radiance shapes refl (drop 7 rands) (depth+1) 1) `muld` rp
                                                            else ( radiance shapes (Ray x tdir) (drop 7 rands) (depth+1) 1) `muld` tp
                                                    else add (( radiance shapes refl (drop 7 rands) (depth+1) 1) `muld` re) (( radiance shapes (Ray x tdir) (drop 14 rands) (depth+1) 1) `muld` tr))
      
-- | Return color of subpixel                                                              
getSubpixel :: [Shape] -> Ray -> [Double] -> Vec -> Int -> Double -> Vec -> Vec -> Double -> Double -> Double -> Double -> Double -> Double -> Vec
getSubpixel scene cam rands acc samps asamps cx cy sx sy x y w h
    | samps == 0 = acc
    | otherwise = 
        let
            r1 = 2*(head rands)
            r2 = 2*(head (drop 1 rands))
            dx = if r1 < 1
                    then sqrt(r1)-1
                    else 1-sqrt(2-r1)
                    
            dy = if r2 < 1
                    then sqrt(r2)-1
                    else 1-sqrt(2-r2)
            sdx = (((sx + 0.5 + dx)/2 + x)/w - 0.5)
            sdy = (((sy + 0.5 + dy)/2 + y)/h - 0.5)
            ddx = cx `muld` sdx
            ddy = cy `muld` sdy
            d = (ddx `add` ( ddy `add` (direction cam)))
            rayori = (origin cam) `add` (d `muld` 140.0)
            newray = (Ray rayori  (norm d))
            rad = newray `seq` ((radiance scene (newray) (drop 2 rands) 0 1.0) `muld` (1.0/asamps))
        in
            getSubpixel scene cam (drop 17 rands) (acc `add` rad) (samps-1) asamps cx cy sx sy x y w h

-- | Return color of pixel
getPixel scene cam rands samps cx cy x y w h = 
    foldl1 (add) [ fun sx sy | sx <- [0,1], sy <- [0,1]]
        where
            fun = \sx sy -> let
                                Vec(x, y, z) = (getSubpixel scene cam (drop (samps*18*(2*sx + sy)) rands) (Vec(0,0,0)) samps (fromIntegral samps) cx cy (fromIntegral sx) (fromIntegral sy) x y w h)
                            in  
                                Vec((clamp x), (clamp y), (clamp z)) `muld` 0.25


-- | Return image (array of colors)
getImage :: [Shape] -> Ray -> [Double] -> Int -> Int -> Int -> Array (Int, Int) Vec           
getImage scene cam rands samps w h =
    let
        cx = Vec((fromIntegral w)*0.5135/(fromIntegral h), 0, 0) --0.5135 defines field of view angle
        cy = ((norm (cx `cross` (direction cam))) `muld` 0.5135)
        gp = \x y -> getPixel scene cam (drop (samps*18*4*(2*x + y)) rands) samps cx cy x y (fromIntegral w) (fromIntegral h)
    in
        cx `seq` cy `seq` (array ((0, 0), (w, h)) [((i, j), (gp i j)) | i <- [0 .. w], j <- [0 .. h]] )
