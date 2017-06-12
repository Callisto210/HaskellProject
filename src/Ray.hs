module Ray where
import Vec

data Ray = Ray
            {
              origin :: Vec
            , direction :: Vec
            } deriving (Show, Eq)
