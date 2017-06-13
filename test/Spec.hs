{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
import Test.QuickCheck
import Test.HUnit
import Ray
import Material
import Vec
import Shapes
import Lib


instance Arbitrary Vec where
	arbitrary = do
		x <- arbitrary
		y <- arbitrary
		z <- arbitrary
		return (Vec(x, y, z))
		
instance Arbitrary Ray where
	arbitrary = do
		origin <- arbitrary
		direction <- arbitrary
		return (Ray origin direction)
		
instance Arbitrary Material where
	--arbitrary = choose (DIFF, SPEC, REFR)
	arbitrary = do return DIFF
		
instance Arbitrary Shape where
	arbitrary = do
		radius <- arbitrary
		position <- arbitrary
		emission <- arbitrary
		color <- arbitrary
		material <- arbitrary
		return (Sphere radius position emission color material)

prop_light :: Shape -> Bool
prop_light s | (emission s) == Vec(0.0, 0.0, 0.0) = ([] == getLightSources [s])
			 | otherwise = ([s] == getLightSources [s])
			 
prop_surface :: Shape -> Ray -> Bool
prop_surface s r = case intersect s r of
					Nothing -> True
					Just(s, d) -> ((getSurfaceNormal s ((origin r) `add` ((direction r) `muld` d)) r) `dot` (direction r)) < 0
					
prop_add :: Vec -> Vec -> Vec -> Bool
prop_add a b c = ((a `add` b) `add` c) == (b `add` (a `add` c))

prop_mulv :: Vec -> Vec -> Bool
prop_mulv a b = (a `mulv` b) == (b `mulv` a)

testIntersect :: Test
testIntersect = 
	TestCase $ assertEqual "intersect" ([(Sphere {	radius = 100000.0, 
													position = Vec (50.0,40.8,-99830.0), 
													emission = Vec (0.0,0.0,0.0), 
													color = Vec (0.0,0.0,0.0), 
													material = DIFF},125.71415057311242)]) 
										(hit 
											[   Sphere (1.0e5)  (Vec( 1e5+1,40.8,81.6))  (Vec(0,0,0))       (Vec(0.75,0.25,0.25))       DIFF,
												Sphere (1.0e5)  (Vec(-1e5+99,40.8,81.6)) (Vec(0,0,0))       (Vec(0.25,0.25,0.75))       DIFF, 
												Sphere (1.0e5)  (Vec(50,40.8, 1e5))      (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
												Sphere (1.0e5)  (Vec(50,40.8,-1e5+170))  (Vec(0,0,0))       (Vec(0,0,0))                DIFF, 
												Sphere (1.0e5)  (Vec(50, 1e5, 81.6))     (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
												Sphere (1.0e5)  (Vec(50,-1e5+81.6,81.6)) (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
												Sphere (16.5)   (Vec(27,16.5,47))        (Vec(0,0,0))       (Vec(1,1,1)`muld`0.999)     SPEC, 
												Sphere (16.5)   (Vec(73,16.5,78))        (Vec(0,0,0))       (Vec(1,1,1)`muld`0.999)     REFR, 
												Sphere (600.0)  (Vec(50,681.33,81.6))    (Vec(12,12,12))    (Vec(0,0,0))                DIFF]
												
												(Ray (Vec(50, 52, 295.6)) ( norm (Vec(0, -0.042612, -1)))))

testIntersect2 :: Test
testIntersect2 = 
	TestCase $ assertEqual "intersect" ([(Sphere {	radius = 100000.0, 
													position = Vec (50.0,100000.0,81.6), 
													emission = Vec (0.0,0.0,0.0), 
													color = Vec (0.75,0.75,0.75), 
													material = DIFF},9206.947527413067)])
										(hit 
											[   Sphere (1.0e5)  (Vec( 1e5+1,40.8,81.6))  (Vec(0,0,0))       (Vec(0.75,0.25,0.25))       DIFF,
												Sphere (1.0e5)  (Vec(-1e5+99,40.8,81.6)) (Vec(0,0,0))       (Vec(0.25,0.25,0.75))       DIFF, 
												Sphere (1.0e5)  (Vec(50,40.8, 1e5))      (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
												Sphere (1.0e5)  (Vec(50,40.8,-1e5+170))  (Vec(0,0,0))       (Vec(0,0,0))                DIFF, 
												Sphere (1.0e5)  (Vec(50, 1e5, 81.6))     (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
												Sphere (1.0e5)  (Vec(50,-1e5+81.6,81.6)) (Vec(0,0,0))       (Vec(0.75,0.75,0.75))       DIFF, 
												Sphere (16.5)   (Vec(27,16.5,47))        (Vec(0,0,0))       (Vec(1,1,1)`muld`0.999)     SPEC, 
												Sphere (16.5)   (Vec(73,16.5,78))        (Vec(0,0,0))       (Vec(1,1,1)`muld`0.999)     REFR, 
												Sphere (600.0)  (Vec(50,681.33,81.6))    (Vec(12,12,12))    (Vec(0,0,0))                DIFF]
												
												(Ray (Vec(50, 52, 295.6)) ( norm (Vec(0, 0.042612, 1)))))




main = do
	runTestTT (TestList [ testIntersect
						, testIntersect2])
	
	quickCheck prop_add
	quickCheck prop_mulv
	quickCheck prop_light
	quickCheck prop_surface
