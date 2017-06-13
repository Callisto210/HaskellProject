import Vec
import Shapes
import Ray
import Material
import Lib
import Data.Maybe
import Rand
import System.Random.MWC
import Data.Array
import Debug.Trace


main = do
    gen <- create
    x <- save gen
    let scene = [   Sphere (1.0e5)  (Vec( 1e5+1,40.8,81.6))  (Vec(0,0,0))       (Vec(0.75,0.25,0.25))       DIFF,
                    Sphere (1.0e5)  (Vec(-1e5+99,40.8,81.6)) (Vec(0,0,0))       (Vec(0.25,0.25,0.75))       DIFF, 
                    Sphere (1.0e5)  (Vec(50,40.8, 1e5))      (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
                    Sphere (1.0e5)  (Vec(50,40.8,-1e5+170))  (Vec(0,0,0))       (Vec(0,0,0))                DIFF, 
                    Sphere (1.0e5)  (Vec(50, 1e5, 81.6))     (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
                    Sphere (1.0e5)  (Vec(50,-1e5+81.6,81.6)) (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
                    Sphere (16.5)   (Vec(27,16.5,47))        (Vec(0,0,0))       (Vec(1,1,1)`muld`0.999)     SPEC, 
                    Sphere (16.5)   (Vec(73,16.5,78))        (Vec(0,0,0))       (Vec(1,1,1)`muld`0.999)     REFR, 
                    Sphere (600.0)  (Vec(50,681.33,81.6))    (Vec(12,12,12))    (Vec(0,0,0))                DIFF]
        cam = (Ray (Vec(50, 52, 295.6)) ( norm (Vec(0, -0.042612, -1))))
        rands = (rList x :: [Double])
        samples = 100
        w = 512
        h = 384
    print (getImage scene cam rands samples w h)
    return (getImage scene cam rands samples w h) :: IO (Array (Int, Int) Vec)
        
        
