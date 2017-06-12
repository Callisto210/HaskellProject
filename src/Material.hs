module Material where

data Material = DIFF | SPEC | REFR deriving (Enum, Show, Eq)


--Diffuse - normal material
--Specular - mirror
--Refraction - glass

